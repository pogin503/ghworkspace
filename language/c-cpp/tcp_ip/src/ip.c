#include "ip.h"
#include "ethernet.h"
#include "arp.h"
#include "icmp.h"
#include "tcp.h"
#include "utils.h"
#include "tun.h"

#include <string.h>
#include <arpa/inet.h>

/* 送信パケットの識別子カウンター */
static uint16_t ip_id_counter = 0;

/*
 * IPv4ヘッダのチェックサムを検証する。
 * RFC 1071: ヘッダ全体 (チェックサムフィールドを含む) の和が0になれば正当。
 */
int ip_checksum_valid(const struct ip_hdr *hdr) {
    size_t hdr_len = (size_t)(hdr->version_ihl & 0x0f) * 4;
    return checksum(hdr, hdr_len) == 0;
}

/* IPv4ヘッダのチェックサムフィールドを計算して埋め込む */
void ip_checksum_fill(struct ip_hdr *hdr) {
    size_t hdr_len = (size_t)(hdr->version_ihl & 0x0f) * 4;
    hdr->checksum = 0;
    hdr->checksum = htons(checksum(hdr, hdr_len));
}

/* 受信したIPv4パケットを解析してプロトコルにディスパッチする */
int ip_recv(struct netdev *dev, uint8_t *pkt, size_t len) {
    if (len < IP_HDR_MIN_LEN) {
        log_err("ip_recv: packet too short (%zu bytes)", len);
        return -1;
    }

    struct ip_hdr *hdr = (struct ip_hdr *)pkt;

    /* IPv4のみ対応 (version == 4) */
    if ((hdr->version_ihl >> 4) != 4) {
        return 0;
    }

    /* チェックサム検証 */
    if (!ip_checksum_valid(hdr)) {
        log_err("ip_recv: invalid checksum");
        return -1;
    }

    /* 自分宛て (unicast) のパケットのみ処理する */
    if (hdr->dst_ip != dev->ip) {
        return 0;
    }

    size_t hdr_len    = (size_t)(hdr->version_ihl & 0x0f) * 4;
    size_t total_len  = (size_t)ntohs(hdr->total_len);

    if (total_len < hdr_len || total_len > len) {
        log_err("ip_recv: invalid total_len=%zu hdr_len=%zu len=%zu",
                total_len, hdr_len, len);
        return -1;
    }

    uint8_t *payload     = pkt + hdr_len;
    size_t   payload_len = total_len - hdr_len;

    switch (hdr->protocol) {
    case IP_PROTO_ICMP:
        return icmp_recv(dev, hdr->src_ip, payload, payload_len);
    case IP_PROTO_TCP:
        return tcp_recv(dev, hdr->src_ip, hdr->dst_ip, payload, payload_len);
    default:
        /* 未対応プロトコルは無視 */
        return 0;
    }
}

/*
 * IPv4パケットを組み立ててEthernetフレームとして送信する。
 * 宛先MACアドレスがARPキャッシュになければARPリクエストを送って今回はドロップ。
 */
int ip_send(struct netdev *dev, uint32_t dst_ip, uint8_t protocol,
            const uint8_t *payload, size_t payload_len) {
    uint8_t pkt[TUN_BUF_SIZE];
    struct ip_hdr *hdr = (struct ip_hdr *)pkt;

    if (IP_HDR_MIN_LEN + payload_len > sizeof(pkt)) {
        log_err("ip_send: payload too large (%zu bytes)", payload_len);
        return -1;
    }

    memset(hdr, 0, IP_HDR_MIN_LEN);
    hdr->version_ihl = (uint8_t)((4 << 4) | 5);  /* version=4, IHL=5 (20 bytes) */
    hdr->dscp_ecn    = 0;
    hdr->total_len   = htons((uint16_t)(IP_HDR_MIN_LEN + payload_len));
    hdr->id          = htons(ip_id_counter);
    ip_id_counter++;
    hdr->flags_frag  = 0;
    hdr->ttl         = IP_DEFAULT_TTL;
    hdr->protocol    = protocol;
    hdr->src_ip      = dev->ip;
    hdr->dst_ip      = dst_ip;

    ip_checksum_fill(hdr);

    memcpy(pkt + IP_HDR_MIN_LEN, payload, payload_len);

    /* ARPキャッシュから宛先MACアドレスを取得する */
    const uint8_t *dst_mac = arp_cache_lookup(dst_ip);
    if (dst_mac == NULL) {
        /* MACが不明: ARPリクエストを送信して今回のパケットはドロップ */
        arp_send_request(dev, dst_ip);
        return -1;
    }

    return eth_send(dev, dst_mac, ETHERTYPE_IP, pkt,
                    IP_HDR_MIN_LEN + payload_len);
}
