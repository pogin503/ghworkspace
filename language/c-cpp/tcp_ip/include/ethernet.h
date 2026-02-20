#ifndef ETHERNET_H
#define ETHERNET_H

#include <stdint.h>
#include <stddef.h>

#define ETH_ALEN       6       /* MACアドレスのバイト長 */
#define ETH_HDR_LEN    14      /* Ethernetヘッダのバイト長 */
#define ETHERTYPE_IP   0x0800  /* IPv4 */
#define ETHERTYPE_ARP  0x0806  /* ARP */

/* ブロードキャストMACアドレス */
extern const uint8_t ETH_BCAST_ADDR[ETH_ALEN];

/* Ethernet IIフレームヘッダ (wire layout) */
struct eth_hdr {
    uint8_t  dst_mac[ETH_ALEN];
    uint8_t  src_mac[ETH_ALEN];
    uint16_t ethertype;  /* ネットワークバイトオーダー */
} __attribute__((packed));

struct netdev;

/* 受信フレームを解析してARP/IPにディスパッチする */
int eth_recv(struct netdev *dev, uint8_t *frame, size_t len);

/* Ethernetフレームを構築してTAPデバイスに書き込む */
int eth_send(struct netdev *dev, const uint8_t *dst_mac,
             uint16_t ethertype, const uint8_t *payload, size_t payload_len);

#endif /* ETHERNET_H */
