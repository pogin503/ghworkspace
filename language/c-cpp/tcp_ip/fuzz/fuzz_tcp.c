/*
 * fuzz_tcp.c - tcp_recv() のファズターゲット (最重要)
 *
 * TCPセグメント (TCPヘッダ + データ) を入力として受け取る。
 *
 * 【チェックサム補正】
 * tcp_recv() は IP擬似ヘッダを含むチェックサムを検証する。
 * チェックサムを再計算して埋め込むことで、state machine の深い分岐を探索する。
 *
 * 【状態リセット】
 * ファズイテレーション間でTCP接続テーブルをリセットし、LISTENソケットを
 * 再作成することで、毎回クリーンな状態から3ウェイハンドシェイク等を試みる。
 * これにより tcp_state_machine() の各状態遷移を均等に探索できる。
 */
#include "tcp.h"
#include "ip.h"
#include "utils.h"
#include "tun.h"

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <arpa/inet.h>

static struct netdev g_dev;
static uint32_t      g_src_ip;
static uint32_t      g_dst_ip;

int LLVMFuzzerInitialize(int *argc, char ***argv) {
    (void)argc;
    (void)argv;

    memset(&g_dev, 0, sizeof(g_dev));
    memcpy(g_dev.mac, "\x00\x0c\x29\x00\x00\x01", 6);
    inet_pton(AF_INET, "10.0.0.2", &g_dev.ip);
    inet_pton(AF_INET, "255.255.255.0", &g_dev.netmask);
    g_dev.fd = 3;

    inet_pton(AF_INET, "10.0.0.1", &g_src_ip);
    g_dst_ip = g_dev.ip;

    /* 固定シードでrand()の再現性を確保 */
    srand(42);

    return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    if (size > 65535) {
        return 0;
    }

    uint8_t buf[65535];
    memcpy(buf, data, size);

    /*
     * イテレーション毎にTCPテーブルをリセットしてLISTENソケットを再作成する。
     * これにより毎回 LISTEN → SYN_RECEIVED → ESTABLISHED の遷移を探索できる。
     */
    tcp_reset_for_fuzz(g_dst_ip, 8080);

    /* TCPヘッダが存在する場合はチェックサムを再計算する */
    if (size >= TCP_HDR_MIN_LEN) {
        struct tcp_hdr *hdr = (struct tcp_hdr *)buf;

        /* IP擬似ヘッダ + TCPセグメント全体のチェックサムを計算 */
        struct ip_pseudo_hdr pseudo;
        pseudo.src_ip   = g_src_ip;
        pseudo.dst_ip   = g_dst_ip;
        pseudo.zero     = 0;
        pseudo.protocol = IP_PROTO_TCP;
        pseudo.tcp_len  = htons((uint16_t)size);

        /* 宛先ポートを必ずLISTENポートに設定: state machine に到達させる */
        hdr->dst_port = htons(8080);

        hdr->checksum = 0;
        uint32_t csum = checksum_partial(&pseudo, sizeof(pseudo), 0);
        csum          = checksum_partial(buf, size, csum);
        hdr->checksum = htons(checksum_finish(csum));
    }

    tcp_recv(&g_dev, g_src_ip, g_dst_ip, buf, size);
    return 0;
}
