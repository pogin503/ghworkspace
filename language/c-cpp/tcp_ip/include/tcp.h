#ifndef TCP_H
#define TCP_H

#include <stdint.h>
#include <stddef.h>

/* TCPフラグビット (RFC 793) */
#define TCP_FLAG_FIN  0x01
#define TCP_FLAG_SYN  0x02
#define TCP_FLAG_RST  0x04
#define TCP_FLAG_PSH  0x08
#define TCP_FLAG_ACK  0x10
#define TCP_FLAG_URG  0x20

#define TCP_HDR_MIN_LEN    20    /* TCPヘッダ最小サイズ (オプションなし) */
#define TCP_WINDOW_SIZE    4096  /* 受信ウィンドウサイズ */
#define TCP_MAX_CONNS      16    /* 最大同時接続数 */
#define TCP_RECV_BUF_SIZE  8192  /* 受信バッファサイズ */

/* TCPヘッダ (wire layout) */
struct tcp_hdr {
    uint16_t src_port;  /* ネットワークバイトオーダー */
    uint16_t dst_port;  /* ネットワークバイトオーダー */
    uint32_t seq_num;   /* ネットワークバイトオーダー */
    uint32_t ack_num;   /* ネットワークバイトオーダー */
    uint8_t  data_off;  /* 上位4ビット = ヘッダ長 (32ビット語単位) */
    uint8_t  flags;
    uint16_t window;    /* ネットワークバイトオーダー */
    uint16_t checksum;  /* ネットワークバイトオーダー */
    uint16_t urg_ptr;   /* ネットワークバイトオーダー */
} __attribute__((packed));

/* RFC 793 TCP状態機械の状態 */
typedef enum {
    TCP_STATE_CLOSED = 0,
    TCP_STATE_LISTEN,
    TCP_STATE_SYN_RECEIVED,
    TCP_STATE_ESTABLISHED,
    TCP_STATE_FIN_WAIT_1,
    TCP_STATE_FIN_WAIT_2,
    TCP_STATE_CLOSE_WAIT,
    TCP_STATE_LAST_ACK,
    TCP_STATE_TIME_WAIT,
} tcp_state_t;

/* TCP接続制御ブロック (TCB: Transmission Control Block) */
struct tcp_conn {
    tcp_state_t state;

    /* 接続を識別する4タプル */
    uint32_t local_ip;
    uint16_t local_port;   /* ホストバイトオーダー */
    uint32_t remote_ip;
    uint16_t remote_port;  /* ホストバイトオーダー */

    /* 送信シーケンス変数 */
    uint32_t snd_una;      /* 未確認の最古シーケンス番号 */
    uint32_t snd_nxt;      /* 次に送信するシーケンス番号 */
    uint16_t snd_wnd;      /* 送信ウィンドウサイズ */

    /* 受信シーケンス変数 */
    uint32_t rcv_nxt;      /* 次に期待するシーケンス番号 */
    uint16_t rcv_wnd;      /* 受信ウィンドウサイズ (送告値) */

    /* 受信バッファ */
    uint8_t  recv_buf[TCP_RECV_BUF_SIZE];
    size_t   recv_len;
};

struct netdev;

/* 受信したTCPセグメントを処理する */
int tcp_recv(struct netdev *dev, uint32_t src_ip, uint32_t dst_ip,
             uint8_t *segment, size_t seg_len);

/* TCP接続上でセグメントを送信する */
int tcp_send_segment(struct netdev *dev, struct tcp_conn *conn,
                     uint8_t flags, const uint8_t *data, size_t data_len);

/* 4タプルで接続を検索する */
struct tcp_conn *tcp_conn_lookup(uint32_t src_ip, uint16_t src_port,
                                 uint32_t dst_ip, uint16_t dst_port);

/* 宛先ポートでLISTENソケットを検索する */
struct tcp_conn *tcp_listen_lookup(uint16_t dst_port);

/* 空きスロットを確保して返す */
struct tcp_conn *tcp_conn_alloc(void);

/* 接続スロットを解放する */
void tcp_conn_free(struct tcp_conn *conn);

/* 指定ポートでLISTEN状態にする (サーバ側) */
int tcp_listen(uint32_t local_ip, uint16_t port);

/* デバッグ用: 全接続状態をダンプする */
void tcp_conn_dump(void);

#ifdef FUZZING
/* ファズテスト専用: 接続テーブルをリセットしてLISTENを再作成する */
void tcp_reset_for_fuzz(uint32_t local_ip, uint16_t port);
#endif

#endif /* TCP_H */
