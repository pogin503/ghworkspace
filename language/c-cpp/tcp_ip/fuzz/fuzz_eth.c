/*
 * fuzz_eth.c - eth_recv() のファズターゲット
 *
 * Ethernetフレーム全体 (14バイトヘッダ + ペイロード) を入力として受け取り、
 * eth_recv() を通じて ARP/IP パーサーまで一気に通す。
 */
#include "ethernet.h"
#include "tun.h"

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <arpa/inet.h>

static struct netdev g_dev;

/* libFuzzer 初期化 (プロセス起動時に1回だけ呼ばれる) */
int LLVMFuzzerInitialize(int *argc, char ***argv) {
    (void)argc;
    (void)argv;

    memset(&g_dev, 0, sizeof(g_dev));
    /* スタックのMACアドレスとIPアドレスを固定値で設定 */
    memcpy(g_dev.mac, "\x00\x0c\x29\x00\x00\x01", 6);
    inet_pton(AF_INET, "10.0.0.2", &g_dev.ip);
    inet_pton(AF_INET, "255.255.255.0", &g_dev.netmask);
    g_dev.fd = 3;
    return 0;
}

/*
 * libFuzzer のエントリーポイント。
 * data/size をそのままEthernetフレームとして eth_recv() に渡す。
 * パーサーはバッファを変更しうるのでmutableコピーを作る。
 */
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    /* スタックバッファはmtに収まるサイズに制限 */
    if (size > 65535) {
        return 0;
    }

    uint8_t buf[65535];
    memcpy(buf, data, size);

    eth_recv(&g_dev, buf, size);
    return 0;
}
