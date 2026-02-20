#include "ethernet.h"
#include "tun.h"
#include "arp.h"
#include "ip.h"
#include "utils.h"

#include <string.h>
#include <arpa/inet.h>

const uint8_t ETH_BCAST_ADDR[ETH_ALEN] = {
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff
};

/* 受信したEthernetフレームをEtherTypeに基づいて各プロトコル処理にディスパッチする */
int eth_recv(struct netdev *dev, uint8_t *frame, size_t len) {
    if (len < ETH_HDR_LEN) {
        log_err("eth_recv: frame too short (%zu bytes)", len);
        return -1;
    }

    struct eth_hdr *hdr = (struct eth_hdr *)frame;
    uint16_t ethertype = (uint16_t)ntohs(hdr->ethertype);

    uint8_t *payload     = frame + ETH_HDR_LEN;
    size_t   payload_len = len - ETH_HDR_LEN;

    switch (ethertype) {
    case ETHERTYPE_ARP:
        return arp_recv(dev, payload, payload_len);
    case ETHERTYPE_IP:
        return ip_recv(dev, payload, payload_len);
    default:
        /* 未知のEtherTypeは無視する */
        return 0;
    }
}

/* Ethernetフレームをスタックバッファに組み立ててTAPデバイスに書き込む */
int eth_send(struct netdev *dev, const uint8_t *dst_mac,
             uint16_t ethertype, const uint8_t *payload, size_t payload_len) {
    uint8_t frame[TUN_BUF_SIZE];
    struct eth_hdr *hdr = (struct eth_hdr *)frame;

    if (ETH_HDR_LEN + payload_len > sizeof(frame)) {
        log_err("eth_send: payload too large (%zu bytes)", payload_len);
        return -1;
    }

    memcpy(hdr->dst_mac, dst_mac, ETH_ALEN);
    memcpy(hdr->src_mac, dev->mac, ETH_ALEN);
    hdr->ethertype = htons(ethertype);

    memcpy(frame + ETH_HDR_LEN, payload, payload_len);

    ssize_t n = tun_write(dev, frame, ETH_HDR_LEN + payload_len);
    return (int)n;
}
