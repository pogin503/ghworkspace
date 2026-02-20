/*
 * fuzz_icmp.c - icmp_recv() のファズターゲット
 *
 * ICMPペイロード (ICMPヘッダ + データ) を入力として受け取る。
 *
 * 【チェックサム補正】
 * icmp_recv() は受信時にチェックサムを検証するため、ランダム入力は
 * チェックサム検証で弾かれる。入力のチェックサムフィールドを再計算して
 * 埋め込むことで、ファザーが type/code の分岐等を探索できるようにする。
 */
#include "icmp.h"
#include "utils.h"
#include "tun.h"

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <arpa/inet.h>

static struct netdev g_dev;
static uint32_t      g_src_ip;

int LLVMFuzzerInitialize(int *argc, char ***argv) {
    (void)argc;
    (void)argv;

    memset(&g_dev, 0, sizeof(g_dev));
    memcpy(g_dev.mac, "\x00\x0c\x29\x00\x00\x01", 6);
    inet_pton(AF_INET, "10.0.0.2", &g_dev.ip);
    inet_pton(AF_INET, "255.255.255.0", &g_dev.netmask);
    inet_pton(AF_INET, "10.0.0.1", &g_src_ip);
    g_dev.fd = 3;
    return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    if (size > 65535) {
        return 0;
    }

    uint8_t buf[65535];
    memcpy(buf, data, size);

    /* ICMPヘッダが存在する場合はチェックサムを再計算する */
    if (size >= sizeof(struct icmp_hdr)) {
        struct icmp_hdr *hdr = (struct icmp_hdr *)buf;
        hdr->checksum = 0;
        hdr->checksum = checksum(buf, size);
    }

    icmp_recv(&g_dev, g_src_ip, buf, size);
    return 0;
}
