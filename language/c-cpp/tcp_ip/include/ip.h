#ifndef IP_H
#define IP_H

#include <stdint.h>
#include <stddef.h>

#define IP_PROTO_ICMP    1   /* ICMPプロトコル番号 */
#define IP_PROTO_TCP     6   /* TCPプロトコル番号 */
#define IP_HDR_MIN_LEN   20  /* オプションなしIPv4ヘッダの最小サイズ */
#define IP_DEFAULT_TTL   64  /* デフォルトTTL値 */

/* IPv4ヘッダ (wire layout, オプションなしを仮定) */
struct ip_hdr {
    uint8_t  version_ihl;   /* version (上位4bit) | IHL (下位4bit) */
    uint8_t  dscp_ecn;
    uint16_t total_len;     /* ネットワークバイトオーダー */
    uint16_t id;            /* ネットワークバイトオーダー */
    uint16_t flags_frag;    /* ネットワークバイトオーダー */
    uint8_t  ttl;
    uint8_t  protocol;
    uint16_t checksum;      /* ネットワークバイトオーダー */
    uint32_t src_ip;        /* ネットワークバイトオーダー */
    uint32_t dst_ip;        /* ネットワークバイトオーダー */
} __attribute__((packed));

/*
 * TCP/UDPチェックサム計算用IP擬似ヘッダ (RFC 793)
 * ネットワークバイトオーダーで格納する
 */
struct ip_pseudo_hdr {
    uint32_t src_ip;
    uint32_t dst_ip;
    uint8_t  zero;
    uint8_t  protocol;
    uint16_t tcp_len;  /* ネットワークバイトオーダー */
} __attribute__((packed));

struct netdev;

/* 受信したIPv4パケットを解析してICMP/TCPにディスパッチする */
int  ip_recv(struct netdev *dev, uint8_t *pkt, size_t len);

/* IPv4パケットを組み立ててEthernetフレームとして送信する */
int  ip_send(struct netdev *dev, uint32_t dst_ip, uint8_t protocol,
             const uint8_t *payload, size_t payload_len);

/* IPv4ヘッダのチェックサムを検証する。有効なら1、無効なら0を返す */
int  ip_checksum_valid(const struct ip_hdr *hdr);

/* IPv4ヘッダのチェックサムフィールドを計算して埋め込む */
void ip_checksum_fill(struct ip_hdr *hdr);

#endif /* IP_H */
