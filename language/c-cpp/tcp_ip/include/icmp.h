#ifndef ICMP_H
#define ICMP_H

#include <stdint.h>
#include <stddef.h>

#define ICMP_TYPE_ECHO_REQUEST  8   /* Echo Request (ping) */
#define ICMP_TYPE_ECHO_REPLY    0   /* Echo Reply */
#define ICMP_CODE_ECHO          0   /* Echo メッセージのコード値 */

/* ICMPヘッダ (wire layout) */
struct icmp_hdr {
    uint8_t  type;
    uint8_t  code;
    uint16_t checksum;  /* ネットワークバイトオーダー */
    uint16_t id;        /* ネットワークバイトオーダー */
    uint16_t seq;       /* ネットワークバイトオーダー */
} __attribute__((packed));

struct netdev;

/* 受信したICMPメッセージを処理する (IPペイロードを指す) */
int icmp_recv(struct netdev *dev, uint32_t src_ip,
              uint8_t *payload, size_t len);

/* ICMP Echo Replyを送信する */
int icmp_send_echo_reply(struct netdev *dev, uint32_t dst_ip,
                         uint16_t id, uint16_t seq,
                         const uint8_t *data, size_t data_len);

#endif /* ICMP_H */
