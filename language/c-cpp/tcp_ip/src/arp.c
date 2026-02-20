#include "arp.h"
#include "ethernet.h"
#include "tun.h"
#include "utils.h"

#include <stdio.h>
#include <string.h>
#include <arpa/inet.h>

/* ARPキャッシュテーブル (静的配列) */
static struct arp_entry cache[ARP_CACHE_SIZE];

/* IPアドレスに対応するキャッシュエントリを更新する。満杯の場合は先頭を上書き */
void arp_cache_update(uint32_t ip, const uint8_t *mac) {
    int free_idx = -1;
    for (int i = 0; i < ARP_CACHE_SIZE; i++) {
        if (cache[i].ip == ip) {
            /* 既存エントリを更新 */
            memcpy(cache[i].mac, mac, 6);
            return;
        }
        if (cache[i].ip == 0 && free_idx == -1) {
            free_idx = i;
        }
    }
    if (free_idx >= 0) {
        cache[free_idx].ip = ip;
        memcpy(cache[free_idx].mac, mac, 6);
    } else {
        /* キャッシュが満杯: 先頭を上書き */
        cache[0].ip = ip;
        memcpy(cache[0].mac, mac, 6);
    }
}

/* IPアドレスに対応するMACアドレスをキャッシュから検索する */
const uint8_t *arp_cache_lookup(uint32_t ip) {
    for (int i = 0; i < ARP_CACHE_SIZE; i++) {
        if (cache[i].ip == ip) {
            return cache[i].mac;
        }
    }
    return NULL;
}

/* ARPパケットを受信して処理する */
int arp_recv(struct netdev *dev, uint8_t *payload, size_t len) {
    if (len < sizeof(struct arp_hdr)) {
        log_err("arp_recv: packet too short (%zu bytes)", len);
        return -1;
    }

    struct arp_hdr *hdr = (struct arp_hdr *)payload;

    /* Ethernet/IPv4 のARPのみを処理する */
    if ((uint16_t)ntohs(hdr->hw_type)    != ARP_HW_ETHER  ||
        (uint16_t)ntohs(hdr->proto_type) != ARP_PROTO_IPV4 ||
        hdr->hw_len    != 6 ||
        hdr->proto_len != 4) {
        return 0;
    }

    /* 送信元のIPアドレス/MACアドレスをキャッシュに登録 */
    arp_cache_update(hdr->sender_ip, hdr->sender_mac);

    uint16_t op = (uint16_t)ntohs(hdr->opcode);

    /* 自分宛てのARPリクエストにのみ応答する */
    if (op == ARP_OP_REQUEST && hdr->target_ip == dev->ip) {
        char sender_str[16], target_str[16];
        struct in_addr a;
        a.s_addr = hdr->sender_ip;
        inet_ntop(AF_INET, &a, sender_str, sizeof(sender_str));
        a.s_addr = hdr->target_ip;
        inet_ntop(AF_INET, &a, target_str, sizeof(target_str));
        log_info("ARP REQUEST: who has %s? tell %s", target_str, sender_str);

        return arp_send_reply(dev, hdr->sender_ip, hdr->sender_mac);
    }

    return 0;
}

/* ARPリプライを送信する */
int arp_send_reply(struct netdev *dev, uint32_t target_ip,
                   const uint8_t *target_mac) {
    struct arp_hdr reply;
    memset(&reply, 0, sizeof(reply));

    reply.hw_type    = htons(ARP_HW_ETHER);
    reply.proto_type = htons(ARP_PROTO_IPV4);
    reply.hw_len     = 6;
    reply.proto_len  = 4;
    reply.opcode     = htons(ARP_OP_REPLY);
    memcpy(reply.sender_mac, dev->mac, 6);
    reply.sender_ip  = dev->ip;
    memcpy(reply.target_mac, target_mac, 6);
    reply.target_ip  = target_ip;

    char target_str[16];
    struct in_addr a;
    a.s_addr = target_ip;
    inet_ntop(AF_INET, &a, target_str, sizeof(target_str));
    log_info("ARP REPLY: %02x:%02x:%02x:%02x:%02x:%02x -> %s",
             dev->mac[0], dev->mac[1], dev->mac[2],
             dev->mac[3], dev->mac[4], dev->mac[5],
             target_str);

    return eth_send(dev, target_mac, ETHERTYPE_ARP,
                    (const uint8_t *)&reply, sizeof(reply));
}

/* ARPリクエストをブロードキャスト送信する */
int arp_send_request(struct netdev *dev, uint32_t target_ip) {
    struct arp_hdr req;
    memset(&req, 0, sizeof(req));

    req.hw_type    = htons(ARP_HW_ETHER);
    req.proto_type = htons(ARP_PROTO_IPV4);
    req.hw_len     = 6;
    req.proto_len  = 4;
    req.opcode     = htons(ARP_OP_REQUEST);
    memcpy(req.sender_mac, dev->mac, 6);
    req.sender_ip  = dev->ip;
    memset(req.target_mac, 0, 6);
    req.target_ip  = target_ip;

    char target_str[16];
    struct in_addr a;
    a.s_addr = target_ip;
    inet_ntop(AF_INET, &a, target_str, sizeof(target_str));
    log_info("ARP REQUEST: who has %s?", target_str);

    return eth_send(dev, ETH_BCAST_ADDR, ETHERTYPE_ARP,
                    (const uint8_t *)&req, sizeof(req));
}

/* ARPキャッシュの内容をデバッグ出力する */
void arp_cache_dump(void) {
    printf("ARP Cache:\n");
    for (int i = 0; i < ARP_CACHE_SIZE; i++) {
        if (cache[i].ip != 0) {
            struct in_addr a;
            a.s_addr = cache[i].ip;
            char ip_str[16];
            inet_ntop(AF_INET, &a, ip_str, sizeof(ip_str));
            const uint8_t *m = cache[i].mac;
            printf("  %s -> %02x:%02x:%02x:%02x:%02x:%02x\n",
                   ip_str, m[0], m[1], m[2], m[3], m[4], m[5]);
        }
    }
}
