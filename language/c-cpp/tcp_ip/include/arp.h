#ifndef ARP_H
#define ARP_H

#include <stdint.h>
#include <stddef.h>

#define ARP_HW_ETHER    1       /* ハードウェアタイプ: Ethernet */
#define ARP_PROTO_IPV4  0x0800  /* プロトコルタイプ: IPv4 */
#define ARP_OP_REQUEST  1       /* ARPリクエスト */
#define ARP_OP_REPLY    2       /* ARPリプライ */
#define ARP_CACHE_SIZE  32      /* ARPキャッシュの最大エントリ数 */

/* Ethernet/IPv4 用ARPパケット (wire layout) */
struct arp_hdr {
    uint16_t hw_type;       /* ネットワークバイトオーダー: 1 = Ethernet */
    uint16_t proto_type;    /* ネットワークバイトオーダー: 0x0800 = IPv4 */
    uint8_t  hw_len;        /* 6 (Ethernet MACの長さ) */
    uint8_t  proto_len;     /* 4 (IPv4アドレスの長さ) */
    uint16_t opcode;        /* ネットワークバイトオーダー: 1=request, 2=reply */
    uint8_t  sender_mac[6];
    uint32_t sender_ip;     /* ネットワークバイトオーダー */
    uint8_t  target_mac[6];
    uint32_t target_ip;     /* ネットワークバイトオーダー */
} __attribute__((packed));

/* ARPキャッシュの1エントリ */
struct arp_entry {
    uint32_t ip;    /* ネットワークバイトオーダー、0 = 空きスロット */
    uint8_t  mac[6];
};

struct netdev;

/* ARPパケットを受信して処理する */
int            arp_recv(struct netdev *dev, uint8_t *payload, size_t len);

/* IPアドレスに対応するMACアドレスをキャッシュから検索する (見つからなければNULL) */
const uint8_t *arp_cache_lookup(uint32_t ip);

/* ARPキャッシュにエントリを追加/更新する */
void           arp_cache_update(uint32_t ip, const uint8_t *mac);

/* 指定IPへのARPリクエストを送信する */
int            arp_send_request(struct netdev *dev, uint32_t target_ip);

/* ARPリプライを送信する (arp_recv から内部的に呼ばれる) */
int            arp_send_reply(struct netdev *dev, uint32_t target_ip,
                              const uint8_t *target_mac);

/* デバッグ用: ARPキャッシュの内容をダンプする */
void           arp_cache_dump(void);

#endif /* ARP_H */
