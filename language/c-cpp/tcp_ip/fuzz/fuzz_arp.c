/*
 * fuzz_arp.c - arp_recv() のファズターゲット
 *
 * ARPパケットのペイロード部分 (Ethernetヘッダ以降) を入力として受け取り、
 * arp_recv() に直接渡す。
 */
#include "arp.h"
#include "tun.h"

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <arpa/inet.h>

static struct netdev g_dev;

int LLVMFuzzerInitialize(int *argc, char ***argv) {
    (void)argc;
    (void)argv;

    memset(&g_dev, 0, sizeof(g_dev));
    memcpy(g_dev.mac, "\x00\x0c\x29\x00\x00\x01", 6);
    inet_pton(AF_INET, "10.0.0.2", &g_dev.ip);
    inet_pton(AF_INET, "255.255.255.0", &g_dev.netmask);
    g_dev.fd = 3;
    return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    if (size > 65535) {
        return 0;
    }

    uint8_t buf[65535];
    memcpy(buf, data, size);

    /* ARPペイロードを直接 arp_recv() に渡す */
    arp_recv(&g_dev, buf, size);
    return 0;
}
