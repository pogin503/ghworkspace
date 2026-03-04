/*
 * test_tcp.c - TCP 状態機械のユニットテスト
 *
 * テスト対象: src/tcp.c
 *   - tcp_listen_lookup()
 *   - tcp_recv() による状態遷移
 *     LISTEN → SYN_RECEIVED → ESTABLISHED
 *
 * setUp() で tcp_reset_for_fuzz() を呼び、毎テストで接続テーブルを
 * クリーンな状態 (LISTENソケットのみ) に戻す。
 * コンパイル時に -DFUZZING が必要。
 */
#include "unity.h"
#include "tcp.h"
#include "ip.h"
#include "utils.h"
#include "tun.h"

#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>

/* テスト用固定アドレス */
static struct netdev g_dev;
static uint32_t     g_local_ip;
static uint32_t     g_remote_ip;

#define LISTEN_PORT  8080
#define REMOTE_PORT  12345

void setUp(void) {
    memset(&g_dev, 0, sizeof(g_dev));
    memcpy(g_dev.mac, "\x00\x0c\x29\x00\x00\x01", 6);
    inet_pton(AF_INET, "10.0.0.2", &g_dev.ip);
    inet_pton(AF_INET, "255.255.255.0", &g_dev.netmask);
    g_dev.fd = 3;

    g_local_ip = g_dev.ip;
    inet_pton(AF_INET, "10.0.0.1", &g_remote_ip);

    /* 接続テーブルをリセットし LISTEN ソケットを再作成 */
    tcp_reset_for_fuzz(g_local_ip, LISTEN_PORT);
}

void tearDown(void) {}

/*
 * チェックサム付き TCP セグメントを buf に書き込む。
 * data_off は 5 (20バイトヘッダ、オプションなし)。
 */
static size_t build_tcp_segment(uint8_t *buf, size_t buf_size,
                                uint16_t src_port, uint16_t dst_port,
                                uint32_t seq, uint32_t ack_seq,
                                uint8_t flags) {
    (void)buf_size;
    struct tcp_hdr *hdr = (struct tcp_hdr *)buf;
    memset(hdr, 0, TCP_HDR_MIN_LEN);

    hdr->src_port = htons(src_port);
    hdr->dst_port = htons(dst_port);
    hdr->seq_num  = htonl(seq);
    hdr->ack_num  = htonl(ack_seq);
    hdr->data_off = (uint8_t)((TCP_HDR_MIN_LEN / 4) << 4);
    hdr->flags    = flags;
    hdr->window   = htons(TCP_WINDOW_SIZE);

    /* IP 擬似ヘッダを含むチェックサムを計算 (tcp_recv が検証する) */
    struct ip_pseudo_hdr pseudo;
    pseudo.src_ip   = g_remote_ip;
    pseudo.dst_ip   = g_local_ip;
    pseudo.zero     = 0;
    pseudo.protocol = IP_PROTO_TCP;
    pseudo.tcp_len  = htons(TCP_HDR_MIN_LEN);

    hdr->checksum = 0;
    uint32_t csum = checksum_partial(&pseudo, sizeof(pseudo), 0);
    csum          = checksum_partial(buf, TCP_HDR_MIN_LEN, csum);
    /* checksum_finish はホストバイトオーダーで返すため、
     * ネットワークバイトオーダー (= checksum_partial が期待する形式) に変換する */
    hdr->checksum = htons(checksum_finish(csum));

    return TCP_HDR_MIN_LEN;
}

/* --- テストケース --- */

/* tcp_reset_for_fuzz() 後に LISTEN ソケットが存在する */
void test_tcp_listen_socket_exists(void) {
    struct tcp_conn *conn = tcp_listen_lookup(LISTEN_PORT);
    TEST_ASSERT_NOT_NULL(conn);
    TEST_ASSERT_EQUAL_INT(TCP_STATE_LISTEN, conn->state);
}

/* SYN 受信後に SYN_RECEIVED へ遷移する */
void test_tcp_syn_to_syn_received(void) {
    uint8_t buf[TCP_HDR_MIN_LEN];
    size_t  len = build_tcp_segment(buf, sizeof(buf),
                                    REMOTE_PORT, LISTEN_PORT,
                                    1000, 0,
                                    TCP_FLAG_SYN);
    tcp_recv(&g_dev, g_remote_ip, g_local_ip, buf, len);

    struct tcp_conn *conn = tcp_conn_lookup(g_remote_ip, REMOTE_PORT,
                                            g_local_ip, LISTEN_PORT);
    TEST_ASSERT_NOT_NULL(conn);
    TEST_ASSERT_EQUAL_INT(TCP_STATE_SYN_RECEIVED, conn->state);
}

/*
 * 3ウェイハンドシェイク完了後に ESTABLISHED へ遷移する
 *
 * SYN (seq=1000)
 *   → サーバが SYN+ACK (ack=1001, seq=ISN) を送信
 * ACK (seq=1001, ack=ISN+1)
 *   → ESTABLISHED
 */
void test_tcp_three_way_handshake(void) {
    uint8_t buf[TCP_HDR_MIN_LEN];
    size_t  len;

    /* Step 1: SYN */
    len = build_tcp_segment(buf, sizeof(buf),
                            REMOTE_PORT, LISTEN_PORT,
                            1000, 0,
                            TCP_FLAG_SYN);
    tcp_recv(&g_dev, g_remote_ip, g_local_ip, buf, len);

    struct tcp_conn *conn = tcp_conn_lookup(g_remote_ip, REMOTE_PORT,
                                            g_local_ip, LISTEN_PORT);
    TEST_ASSERT_NOT_NULL(conn);
    TEST_ASSERT_EQUAL_INT(TCP_STATE_SYN_RECEIVED, conn->state);

    /* Step 2: ACK (サーバが送った SYN+ACK に対する確認)
     * snd_nxt は SYN+ACK 送信後に ISN+1 になっている */
    uint32_t server_isn_plus1 = conn->snd_nxt;

    len = build_tcp_segment(buf, sizeof(buf),
                            REMOTE_PORT, LISTEN_PORT,
                            1001, server_isn_plus1,
                            TCP_FLAG_ACK);
    tcp_recv(&g_dev, g_remote_ip, g_local_ip, buf, len);

    conn = tcp_conn_lookup(g_remote_ip, REMOTE_PORT,
                           g_local_ip, LISTEN_PORT);
    TEST_ASSERT_NOT_NULL(conn);
    TEST_ASSERT_EQUAL_INT(TCP_STATE_ESTABLISHED, conn->state);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_tcp_listen_socket_exists);
    RUN_TEST(test_tcp_syn_to_syn_received);
    RUN_TEST(test_tcp_three_way_handshake);
    return UNITY_END();
}
