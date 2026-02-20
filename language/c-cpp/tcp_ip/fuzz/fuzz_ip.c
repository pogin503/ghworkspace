/*
 * fuzz_ip.c - ip_recv() のファズターゲット
 *
 * IPv4パケット (IPヘッダ + ペイロード) を入力として受け取る。
 *
 * 【チェックサム補正】
 * ランダム入力ではIPヘッダチェックサムが不正になり、パーサー入口で弾かれる。
 * ファザーが深いコードパスを探索できるよう、入力のチェックサムフィールドを
 * 再計算して埋め込んでから ip_recv() を呼ぶ。
 *
 * 【宛先IP補正】
 * ip_recv() は dst_ip != dev->ip のパケットを無視する。
 * 最小20バイト以上の入力に対して dst_ip フィールドを dev->ip に上書きして
 * 送信先フィルタを通過させる。
 */
#include "ip.h"
#include "tun.h"
#include "utils.h"

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

    /* IPヘッダが最小サイズ以上なら補正を適用する */
    if (size >= IP_HDR_MIN_LEN) {
        struct ip_hdr *hdr = (struct ip_hdr *)buf;

        /* 宛先IPを自スタックのIPに補正: 宛先フィルタを通過させる */
        hdr->dst_ip = g_dev.ip;

        /* IPバージョンを4に設定 (version/IHL フィールドの上位4ビット) */
        hdr->version_ihl = (uint8_t)((hdr->version_ihl & 0x0f) | (4 << 4));

        /* IHL が0の場合は最小値5 (20バイト) に補正 */
        if ((hdr->version_ihl & 0x0f) == 0) {
            hdr->version_ihl = (uint8_t)(hdr->version_ihl | 5);
        }

        /* total_len を実際のバッファサイズに合わせる */
        hdr->total_len = htons((uint16_t)size);

        /* IPヘッダチェックサムを再計算: ファザーが深い分岐を探索できるようにする */
        ip_checksum_fill(hdr);
    }

    ip_recv(&g_dev, buf, size);
    return 0;
}
