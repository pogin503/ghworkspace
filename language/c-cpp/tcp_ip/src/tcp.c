#include "tcp.h"
#include "ip.h"
#include "utils.h"
#include "tun.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <arpa/inet.h>

/* TCP接続テーブル (state == CLOSED が空きスロット) */
static struct tcp_conn tcp_conns[TCP_MAX_CONNS];

#ifdef FUZZING
/*
 * ファズテスト専用: 接続テーブルを全てCLOSEDにリセットしてLISTENを再作成する。
 * LLVMFuzzerTestOneInput の先頭で呼ぶことで、毎回クリーンな状態から試験できる。
 */
void tcp_reset_for_fuzz(uint32_t local_ip, uint16_t port) {
    memset(tcp_conns, 0, sizeof(tcp_conns));
    tcp_listen(local_ip, port);
}
#endif

/* デバッグ用: 状態名を文字列で返す */
static const char *state_name(tcp_state_t s) {
    switch (s) {
    case TCP_STATE_CLOSED:       return "CLOSED";
    case TCP_STATE_LISTEN:       return "LISTEN";
    case TCP_STATE_SYN_RECEIVED: return "SYN_RECEIVED";
    case TCP_STATE_ESTABLISHED:  return "ESTABLISHED";
    case TCP_STATE_FIN_WAIT_1:   return "FIN_WAIT_1";
    case TCP_STATE_FIN_WAIT_2:   return "FIN_WAIT_2";
    case TCP_STATE_CLOSE_WAIT:   return "CLOSE_WAIT";
    case TCP_STATE_LAST_ACK:     return "LAST_ACK";
    case TCP_STATE_TIME_WAIT:    return "TIME_WAIT";
    default:                     return "UNKNOWN";
    }
}

/* CLOSED状態のスロットを確保してゼロ初期化する */
struct tcp_conn *tcp_conn_alloc(void) {
    for (int i = 0; i < TCP_MAX_CONNS; i++) {
        if (tcp_conns[i].state == TCP_STATE_CLOSED) {
            memset(&tcp_conns[i], 0, sizeof(tcp_conns[i]));
            return &tcp_conns[i];
        }
    }
    return NULL;
}

/* 接続スロットをCLOSED状態に戻す */
void tcp_conn_free(struct tcp_conn *conn) {
    conn->state = TCP_STATE_CLOSED;
}

/* 4タプルで既存接続を検索する */
struct tcp_conn *tcp_conn_lookup(uint32_t src_ip, uint16_t src_port,
                                 uint32_t dst_ip, uint16_t dst_port) {
    for (int i = 0; i < TCP_MAX_CONNS; i++) {
        struct tcp_conn *c = &tcp_conns[i];
        if (c->state != TCP_STATE_CLOSED &&
            c->state != TCP_STATE_LISTEN &&
            c->remote_ip   == src_ip   &&
            c->remote_port == src_port &&
            c->local_ip    == dst_ip   &&
            c->local_port  == dst_port) {
            return c;
        }
    }
    return NULL;
}

/* 宛先ポートのLISTENソケットを検索する */
struct tcp_conn *tcp_listen_lookup(uint16_t dst_port) {
    for (int i = 0; i < TCP_MAX_CONNS; i++) {
        if (tcp_conns[i].state == TCP_STATE_LISTEN &&
            tcp_conns[i].local_port == dst_port) {
            return &tcp_conns[i];
        }
    }
    return NULL;
}

/* 指定ポートでLISTEN状態にする */
int tcp_listen(uint32_t local_ip, uint16_t port) {
    struct tcp_conn *conn = tcp_conn_alloc();
    if (conn == NULL) {
        log_err("tcp_listen: no free connections");
        return -1;
    }
    conn->state      = TCP_STATE_LISTEN;
    conn->local_ip   = local_ip;
    conn->local_port = port;
    conn->rcv_wnd    = TCP_WINDOW_SIZE;
    log_info("TCP LISTEN on port %u", (unsigned)port);
    return 0;
}

/*
 * TCPセグメントを送信する。
 * IPヘッダ擬似ヘッダを使ったチェックサムを計算してip_send()に渡す。
 * SYN/FINフラグはシーケンス番号を1消費する (RFC 793)。
 */
int tcp_send_segment(struct netdev *dev, struct tcp_conn *conn,
                     uint8_t flags, const uint8_t *data, size_t data_len) {
    size_t   seg_len = TCP_HDR_MIN_LEN + data_len;
    uint8_t *seg     = malloc(seg_len);
    if (seg == NULL) {
        log_err("tcp_send_segment: malloc failed");
        return -1;
    }

    struct tcp_hdr *hdr = (struct tcp_hdr *)seg;
    memset(hdr, 0, TCP_HDR_MIN_LEN);
    hdr->src_port = htons(conn->local_port);
    hdr->dst_port = htons(conn->remote_port);
    hdr->seq_num  = htonl(conn->snd_nxt);
    hdr->ack_num  = (flags & TCP_FLAG_ACK) ? htonl(conn->rcv_nxt) : 0;
    hdr->data_off = (uint8_t)((TCP_HDR_MIN_LEN / 4) << 4);
    hdr->flags    = flags;
    hdr->window   = htons(conn->rcv_wnd);
    hdr->checksum = 0;
    hdr->urg_ptr  = 0;

    if (data != NULL && data_len > 0) {
        memcpy(seg + TCP_HDR_MIN_LEN, data, data_len);
    }

    /* TCP チェックサム: IP擬似ヘッダ + TCPセグメント全体を対象とする */
    struct ip_pseudo_hdr pseudo;
    pseudo.src_ip   = conn->local_ip;
    pseudo.dst_ip   = conn->remote_ip;
    pseudo.zero     = 0;
    pseudo.protocol = IP_PROTO_TCP;
    pseudo.tcp_len  = htons((uint16_t)seg_len);

    uint32_t csum = checksum_partial(&pseudo, sizeof(pseudo), 0);
    csum          = checksum_partial(seg, seg_len, csum);
    hdr->checksum = checksum_finish(csum);

    /* SYN/FINフラグはシーケンス番号を1消費する */
    uint32_t consumed = (uint32_t)data_len;
    if (flags & TCP_FLAG_SYN) consumed++;
    if (flags & TCP_FLAG_FIN) consumed++;
    conn->snd_nxt += consumed;

    int ret = ip_send(dev, conn->remote_ip, IP_PROTO_TCP, seg, seg_len);
    free(seg);
    return ret;
}

/*
 * RFC 793 TCP状態機械。
 * 各状態でフラグに応じた遷移を行い、必要なセグメントを送信する。
 */
static int tcp_state_machine(struct netdev *dev, struct tcp_conn *conn,
                              const struct tcp_hdr *hdr,
                              const uint8_t *data, size_t data_len,
                              uint32_t src_ip) {
    uint8_t  flags = hdr->flags;
    uint32_t seq   = ntohl(hdr->seq_num);
    uint32_t ack   = ntohl(hdr->ack_num);

    switch (conn->state) {
    case TCP_STATE_LISTEN:
        /* SYNを受信したら新しい接続を作成してSYN+ACKを返す */
        if (flags & TCP_FLAG_SYN) {
            struct tcp_conn *new_conn = tcp_conn_alloc();
            if (new_conn == NULL) {
                log_err("tcp: no free connections for incoming SYN");
                return -1;
            }
            new_conn->state       = TCP_STATE_SYN_RECEIVED;
            new_conn->local_ip    = conn->local_ip;
            new_conn->local_port  = conn->local_port;
            new_conn->remote_ip   = src_ip;
            new_conn->remote_port = (uint16_t)ntohs(hdr->src_port);
            new_conn->rcv_nxt     = seq + 1;
            new_conn->rcv_wnd     = TCP_WINDOW_SIZE;
            /* 初期送信シーケンス番号をランダムに設定 */
            new_conn->snd_nxt = (uint32_t)rand();
            new_conn->snd_una = new_conn->snd_nxt;
            new_conn->snd_wnd = (uint16_t)ntohs(hdr->window);

            char src_str[16];
            struct in_addr a;
            a.s_addr = src_ip;
            inet_ntop(AF_INET, &a, src_str, sizeof(src_str));
            log_info("TCP SYN from %s:%u -> port %u [LISTEN -> SYN_RECEIVED]",
                     src_str, (unsigned)new_conn->remote_port,
                     (unsigned)new_conn->local_port);

            return tcp_send_segment(dev, new_conn,
                                    TCP_FLAG_SYN | TCP_FLAG_ACK, NULL, 0);
        }
        break;

    case TCP_STATE_SYN_RECEIVED:
        /* SYN+ACKに対するACKを受信したらESTABLISHEDへ遷移 */
        if ((flags & TCP_FLAG_ACK) && !(flags & TCP_FLAG_SYN)) {
            if (ack == conn->snd_nxt) {
                conn->state   = TCP_STATE_ESTABLISHED;
                conn->snd_una = ack;
                log_info("TCP ESTABLISHED (local=%u remote=%u)",
                         (unsigned)conn->local_port,
                         (unsigned)conn->remote_port);
            }
        }
        break;

    case TCP_STATE_ESTABLISHED:
        if (flags & TCP_FLAG_RST) {
            log_info("TCP RST received, closing (local=%u remote=%u)",
                     (unsigned)conn->local_port, (unsigned)conn->remote_port);
            tcp_conn_free(conn);
            return 0;
        }

        /* 相手側からFINを受信: パッシブクローズ開始 */
        if (flags & TCP_FLAG_FIN) {
            conn->rcv_nxt = seq + 1;
            log_info("TCP FIN received (local=%u) [ESTABLISHED -> LAST_ACK]",
                     (unsigned)conn->local_port);
            /* ACKを送信してからFINを送信し、LAST_ACKへ遷移 */
            conn->state = TCP_STATE_CLOSE_WAIT;
            int r = tcp_send_segment(dev, conn, TCP_FLAG_ACK, NULL, 0);
            conn->state = TCP_STATE_LAST_ACK;
            r |= tcp_send_segment(dev, conn, TCP_FLAG_FIN | TCP_FLAG_ACK,
                                  NULL, 0);
            return r;
        }

        /* データ受信: ACKを返して受信バッファに格納 */
        if (data_len > 0) {
            conn->rcv_nxt = seq + (uint32_t)data_len;

            if (conn->recv_len + data_len <= TCP_RECV_BUF_SIZE) {
                memcpy(conn->recv_buf + conn->recv_len, data, data_len);
                conn->recv_len += data_len;
            } else {
                log_err("tcp: recv buffer full, data dropped");
            }

            log_info("TCP DATA %zu bytes received (local=%u)",
                     data_len, (unsigned)conn->local_port);

            return tcp_send_segment(dev, conn, TCP_FLAG_ACK, NULL, 0);
        }

        /* 純粋なACK: snd_una を更新 */
        if (flags & TCP_FLAG_ACK) {
            conn->snd_una = ack;
        }
        break;

    case TCP_STATE_LAST_ACK:
        /* LAST_ACKへのACKを受信したらCLOSEDへ遷移 */
        if (flags & TCP_FLAG_ACK) {
            log_info("TCP LAST_ACK -> CLOSED (local=%u)",
                     (unsigned)conn->local_port);
            tcp_conn_free(conn);
        }
        break;

    case TCP_STATE_FIN_WAIT_1:
        if (flags & TCP_FLAG_ACK) {
            conn->state = TCP_STATE_FIN_WAIT_2;
        }
        if (flags & TCP_FLAG_FIN) {
            conn->rcv_nxt = seq + 1;
            tcp_send_segment(dev, conn, TCP_FLAG_ACK, NULL, 0);
            /* 簡略化: TIME_WAITを省略してCLOSEDへ */
            tcp_conn_free(conn);
        }
        break;

    case TCP_STATE_FIN_WAIT_2:
        if (flags & TCP_FLAG_FIN) {
            conn->rcv_nxt = seq + 1;
            tcp_send_segment(dev, conn, TCP_FLAG_ACK, NULL, 0);
            /* 簡略化: TIME_WAITを省略してCLOSEDへ */
            tcp_conn_free(conn);
        }
        break;

    default:
        break;
    }

    return 0;
}

/* 受信したTCPセグメントのエントリーポイント */
int tcp_recv(struct netdev *dev, uint32_t src_ip, uint32_t dst_ip,
             uint8_t *segment, size_t seg_len) {
    if (seg_len < TCP_HDR_MIN_LEN) {
        log_err("tcp_recv: segment too short (%zu bytes)", seg_len);
        return -1;
    }

    struct tcp_hdr *hdr = (struct tcp_hdr *)segment;

    /* TCPチェックサム検証 (IP擬似ヘッダ + TCPセグメント) */
    struct ip_pseudo_hdr pseudo;
    pseudo.src_ip   = src_ip;
    pseudo.dst_ip   = dst_ip;
    pseudo.zero     = 0;
    pseudo.protocol = IP_PROTO_TCP;
    pseudo.tcp_len  = htons((uint16_t)seg_len);

    uint32_t csum = checksum_partial(&pseudo, sizeof(pseudo), 0);
    csum          = checksum_partial(segment, seg_len, csum);
    if (checksum_finish(csum) != 0) {
        log_err("tcp_recv: invalid checksum");
        return -1;
    }

    uint16_t src_port = (uint16_t)ntohs(hdr->src_port);
    uint16_t dst_port = (uint16_t)ntohs(hdr->dst_port);

    size_t         hdr_len  = (size_t)(hdr->data_off >> 4) * 4;
    const uint8_t *data     = segment + hdr_len;
    size_t         data_len = seg_len > hdr_len ? seg_len - hdr_len : 0;

    /* まず既存接続を検索し、なければLISTENソケットを検索 */
    struct tcp_conn *conn = tcp_conn_lookup(src_ip, src_port, dst_ip, dst_port);
    if (conn == NULL) {
        conn = tcp_listen_lookup(dst_port);
    }

    if (conn == NULL) {
        log_err("tcp_recv: no connection for port %u", (unsigned)dst_port);
        return -1;
    }

    return tcp_state_machine(dev, conn, hdr, data, data_len, src_ip);
}

/* 全接続状態をデバッグ出力する */
void tcp_conn_dump(void) {
    printf("TCP Connections:\n");
    for (int i = 0; i < TCP_MAX_CONNS; i++) {
        struct tcp_conn *c = &tcp_conns[i];
        if (c->state != TCP_STATE_CLOSED) {
            char local_str[16], remote_str[16];
            struct in_addr a;
            a.s_addr = c->local_ip;
            inet_ntop(AF_INET, &a, local_str, sizeof(local_str));
            a.s_addr = c->remote_ip;
            inet_ntop(AF_INET, &a, remote_str, sizeof(remote_str));
            printf("  %s:%u <-> %s:%u [%s]\n",
                   local_str, (unsigned)c->local_port,
                   remote_str, (unsigned)c->remote_port,
                   state_name(c->state));
        }
    }
}
