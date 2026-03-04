#include "icmp.h"
#include "ip.h"
#include "utils.h"

#include <string.h>
#include <stdlib.h>
#include <arpa/inet.h>

/* 受信したICMPパケットを処理する */
int icmp_recv(struct netdev *dev, uint32_t src_ip,
              uint8_t *payload, size_t len) {
    if (len < sizeof(struct icmp_hdr)) {
        log_err("icmp_recv: packet too short (%zu bytes)", len);
        return -1;
    }

    struct icmp_hdr *hdr = (struct icmp_hdr *)payload;

    /* チェックサム検証: ICMPヘッダ+データ全体の和が0になれば正当 */
    if (checksum(payload, len) != 0) {
        log_err("icmp_recv: invalid checksum");
        return -1;
    }

    if (hdr->type == ICMP_TYPE_ECHO_REQUEST && hdr->code == ICMP_CODE_ECHO) {
        char src_str[16];
        struct in_addr a;
        a.s_addr = src_ip;
        inet_ntop(AF_INET, &a, src_str, sizeof(src_str));
        log_info("ICMP ECHO REQUEST id=%u seq=%u from %s",
                 (unsigned)ntohs(hdr->id), (unsigned)ntohs(hdr->seq), src_str);

        const uint8_t *data     = payload + sizeof(struct icmp_hdr);
        size_t         data_len = len - sizeof(struct icmp_hdr);

        return icmp_send_echo_reply(dev, src_ip,
                                    ntohs(hdr->id), ntohs(hdr->seq),
                                    data, data_len);
    }

    return 0;
}

/* ICMP Echo Replyを送信する */
int icmp_send_echo_reply(struct netdev *dev, uint32_t dst_ip,
                         uint16_t id, uint16_t seq,
                         const uint8_t *data, size_t data_len) {
    size_t   total_len = sizeof(struct icmp_hdr) + data_len;
    uint8_t *buf       = malloc(total_len);
    if (buf == NULL) {
        log_err("icmp_send_echo_reply: malloc failed");
        return -1;
    }

    struct icmp_hdr *hdr = (struct icmp_hdr *)buf;
    hdr->type     = ICMP_TYPE_ECHO_REPLY;
    hdr->code     = ICMP_CODE_ECHO;
    hdr->checksum = 0;
    hdr->id       = htons(id);
    hdr->seq      = htons(seq);

    if (data_len > 0) {
        memcpy(buf + sizeof(struct icmp_hdr), data, data_len);
    }

    /* ICMPチェックサム: ヘッダ+データ全体を対象とする */
    hdr->checksum = htons(checksum(buf, total_len));

    log_info("ICMP ECHO REPLY sent id=%u seq=%u", (unsigned)id, (unsigned)seq);

    int ret = ip_send(dev, dst_ip, IP_PROTO_ICMP, buf, total_len);
    free(buf);
    return ret;
}
