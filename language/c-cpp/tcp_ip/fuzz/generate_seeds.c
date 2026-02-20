/*
 * generate_seeds.c - ファズテスト用シードコーパスの生成
 *
 * 各プロトコル層の有効なパケットをバイナリファイルとして corpus/ 以下に出力する。
 * ファザーはこれらのシードを初期入力として使い、そこから変異させていく。
 *
 * ビルド: gcc -Wall -I../include -o generate_seeds generate_seeds.c
 * 実行:   ./generate_seeds  → corpus/{eth,arp,ip,icmp,tcp}/ にファイルが生成される
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <arpa/inet.h>
#include <sys/stat.h>

/* ============================================================
 * インラインでプロトコルヘッダを定義 (include からの依存を最小化)
 * ============================================================ */

/* RFC 1071 Internet チェックサム */
static uint16_t cksum(const void *data, size_t len) {
    const uint8_t *p = (const uint8_t *)data;
    uint32_t sum = 0;
    while (len > 1) {
        sum += ((uint32_t)p[0] << 8) | p[1];
        p += 2; len -= 2;
    }
    if (len == 1) sum += (uint32_t)p[0] << 8;
    while (sum >> 16) sum = (sum & 0xffff) + (sum >> 16);
    return (uint16_t)~sum;
}

/* ファイルをバイナリで書き出す */
static void write_seed(const char *path, const uint8_t *buf, size_t len) {
    FILE *f = fopen(path, "wb");
    if (f == NULL) {
        perror(path);
        exit(1);
    }
    if (fwrite(buf, 1, len, f) != len) {
        perror("fwrite");
        fclose(f);
        exit(1);
    }
    fclose(f);
    printf("  wrote %zu bytes -> %s\n", len, path);
}

/* ============================================================
 * Ethernet フレームシード
 * ============================================================ */
static void gen_eth_seeds(void) {
    printf("[eth]\n");

    /* シード1: ARPリクエストを含むEthernetフレーム (42バイト) */
    {
        uint8_t frame[42];
        memset(frame, 0, sizeof(frame));

        /* Ethernetヘッダ */
        memset(frame + 0, 0xff, 6);                               /* dst: ブロードキャスト */
        memcpy(frame + 6,  "\x00\x0c\x29\x01\x00\x01", 6);       /* src: 送信元MAC */
        frame[12] = 0x08; frame[13] = 0x06;                       /* EtherType: ARP */

        /* ARPヘッダ */
        frame[14] = 0x00; frame[15] = 0x01;  /* hw_type: Ethernet */
        frame[16] = 0x08; frame[17] = 0x00;  /* proto_type: IPv4 */
        frame[18] = 6;                         /* hw_len */
        frame[19] = 4;                         /* proto_len */
        frame[20] = 0x00; frame[21] = 0x01;  /* opcode: REQUEST */
        memcpy(frame + 22, "\x00\x0c\x29\x01\x00\x01", 6); /* sender MAC */
        frame[26] = 10; frame[27] = 0; frame[28] = 0; frame[29] = 1; /* sender IP: 10.0.0.1 */
        memset(frame + 30, 0, 6);             /* target MAC: 不明 */
        frame[36] = 10; frame[37] = 0; frame[38] = 0; frame[39] = 2; /* target IP: 10.0.0.2 */

        write_seed("corpus/eth/arp_request.bin", frame, sizeof(frame));
    }

    /* シード2: ICMP Echo Requestを含むEthernetフレーム */
    {
        uint8_t frame[56];
        memset(frame, 0, sizeof(frame));

        /* Ethernetヘッダ */
        memcpy(frame + 0, "\x00\x0c\x29\x00\x00\x01", 6); /* dst: スタックMAC */
        memcpy(frame + 6, "\x00\x0c\x29\x01\x00\x01", 6); /* src */
        frame[12] = 0x08; frame[13] = 0x00;                /* EtherType: IP */

        /* IPヘッダ */
        frame[14] = 0x45;  /* version=4, IHL=5 */
        frame[15] = 0;
        frame[16] = 0; frame[17] = 28;  /* total_len = 28 (IP20 + ICMP8) */
        frame[18] = 0; frame[19] = 1;   /* id */
        frame[20] = 0; frame[21] = 0;   /* flags, frag */
        frame[22] = 64;                  /* ttl */
        frame[23] = 1;                   /* protocol: ICMP */
        frame[24] = 0; frame[25] = 0;   /* checksum (fill below) */
        frame[26] = 10; frame[27] = 0; frame[28] = 0; frame[29] = 1; /* src: 10.0.0.1 */
        frame[30] = 10; frame[31] = 0; frame[32] = 0; frame[33] = 2; /* dst: 10.0.0.2 */
        uint16_t ip_ck = cksum(frame + 14, 20);
        frame[24] = (uint8_t)(ip_ck >> 8);
        frame[25] = (uint8_t)(ip_ck & 0xff);

        /* ICMPヘッダ */
        frame[34] = 8; frame[35] = 0;   /* type: ECHO REQUEST, code: 0 */
        frame[36] = 0; frame[37] = 0;   /* checksum (fill below) */
        frame[38] = 0; frame[39] = 1;   /* id */
        frame[40] = 0; frame[41] = 1;   /* seq */
        /* data: 8バイトのダミーペイロード */
        memcpy(frame + 42, "FUZZDATA", 8);
        uint16_t icmp_ck = cksum(frame + 34, 14);
        frame[36] = (uint8_t)(icmp_ck >> 8);
        frame[37] = (uint8_t)(icmp_ck & 0xff);

        write_seed("corpus/eth/icmp_echo_request.bin", frame, sizeof(frame));
    }
}

/* ============================================================
 * ARP ペイロードシード
 * ============================================================ */
static void gen_arp_seeds(void) {
    printf("[arp]\n");

    /* シード1: ARPリクエスト (28バイト) */
    {
        uint8_t pkt[28];
        memset(pkt, 0, sizeof(pkt));
        pkt[0] = 0x00; pkt[1] = 0x01;  /* hw_type: Ethernet */
        pkt[2] = 0x08; pkt[3] = 0x00;  /* proto_type: IPv4 */
        pkt[4] = 6;                      /* hw_len */
        pkt[5] = 4;                      /* proto_len */
        pkt[6] = 0x00; pkt[7] = 0x01;  /* opcode: REQUEST */
        memcpy(pkt + 8, "\x00\x0c\x29\x01\x00\x01", 6); /* sender MAC */
        pkt[14] = 10; pkt[15] = 0; pkt[16] = 0; pkt[17] = 1; /* sender IP */
        memset(pkt + 18, 0, 6);          /* target MAC */
        pkt[24] = 10; pkt[25] = 0; pkt[26] = 0; pkt[27] = 2; /* target IP */
        write_seed("corpus/arp/arp_request.bin", pkt, sizeof(pkt));
    }

    /* シード2: ARPリプライ */
    {
        uint8_t pkt[28];
        memset(pkt, 0, sizeof(pkt));
        pkt[0] = 0x00; pkt[1] = 0x01;
        pkt[2] = 0x08; pkt[3] = 0x00;
        pkt[4] = 6; pkt[5] = 4;
        pkt[6] = 0x00; pkt[7] = 0x02;  /* opcode: REPLY */
        memcpy(pkt + 8,  "\x00\x0c\x29\x01\x00\x01", 6);
        pkt[14] = 10; pkt[15] = 0; pkt[16] = 0; pkt[17] = 1;
        memcpy(pkt + 18, "\x00\x0c\x29\x00\x00\x01", 6);
        pkt[24] = 10; pkt[25] = 0; pkt[26] = 0; pkt[27] = 2;
        write_seed("corpus/arp/arp_reply.bin", pkt, sizeof(pkt));
    }
}

/* ============================================================
 * IP パケットシード
 * ============================================================ */
static void gen_ip_seeds(void) {
    printf("[ip]\n");

    /* シード1: ICMP Echo Requestを含む最小IPパケット */
    {
        uint8_t pkt[28];
        memset(pkt, 0, sizeof(pkt));
        pkt[0] = 0x45;                   /* version=4, IHL=5 */
        pkt[1] = 0;
        pkt[2] = 0; pkt[3] = 28;        /* total_len */
        pkt[4] = 0; pkt[5] = 1;
        pkt[6] = 0; pkt[7] = 0;
        pkt[8] = 64;                     /* ttl */
        pkt[9] = 1;                      /* protocol: ICMP */
        pkt[10] = 0; pkt[11] = 0;       /* checksum */
        pkt[12] = 10; pkt[13] = 0; pkt[14] = 0; pkt[15] = 1;  /* src */
        pkt[16] = 10; pkt[17] = 0; pkt[18] = 0; pkt[19] = 2;  /* dst */
        uint16_t ck = cksum(pkt, 20);
        pkt[10] = (uint8_t)(ck >> 8); pkt[11] = (uint8_t)(ck & 0xff);

        /* ICMPペイロード */
        pkt[20] = 8; pkt[21] = 0;       /* type: ECHO REQUEST */
        pkt[22] = 0; pkt[23] = 0;       /* checksum */
        pkt[24] = 0; pkt[25] = 1;       /* id */
        pkt[26] = 0; pkt[27] = 1;       /* seq */
        uint16_t icmp_ck = cksum(pkt + 20, 8);
        pkt[22] = (uint8_t)(icmp_ck >> 8);
        pkt[23] = (uint8_t)(icmp_ck & 0xff);

        write_seed("corpus/ip/icmp_echo.bin", pkt, sizeof(pkt));
    }

    /* シード2: TCP SYNを含むIPパケット */
    {
        uint8_t pkt[40];
        memset(pkt, 0, sizeof(pkt));
        pkt[0] = 0x45;
        pkt[1] = 0;
        pkt[2] = 0; pkt[3] = 40;       /* total_len = IP20 + TCP20 */
        pkt[4] = 0; pkt[5] = 2;
        pkt[6] = 0; pkt[7] = 0;
        pkt[8] = 64;
        pkt[9] = 6;                      /* protocol: TCP */
        pkt[10] = 0; pkt[11] = 0;
        pkt[12] = 10; pkt[13] = 0; pkt[14] = 0; pkt[15] = 1;  /* src */
        pkt[16] = 10; pkt[17] = 0; pkt[18] = 0; pkt[19] = 2;  /* dst */
        uint16_t ck = cksum(pkt, 20);
        pkt[10] = (uint8_t)(ck >> 8); pkt[11] = (uint8_t)(ck & 0xff);

        /* TCPヘッダ */
        pkt[20] = 0xc0; pkt[21] = 0x00; /* src_port: 49152 */
        pkt[22] = 0x1f; pkt[23] = 0x90; /* dst_port: 8080 */
        pkt[24] = 0; pkt[25] = 0; pkt[26] = 0; pkt[27] = 1; /* seq */
        pkt[28] = 0; pkt[29] = 0; pkt[30] = 0; pkt[31] = 0; /* ack */
        pkt[32] = 0x50;                  /* data_off: 5 (20バイト) */
        pkt[33] = 0x02;                  /* flags: SYN */
        pkt[34] = 0x10; pkt[35] = 0x00; /* window: 4096 */
        pkt[36] = 0; pkt[37] = 0;       /* checksum */
        pkt[38] = 0; pkt[39] = 0;       /* urg_ptr */
        /* TCP checksum with pseudo-header */
        uint8_t pseudo[12];
        memcpy(pseudo, pkt + 12, 4);     /* src_ip */
        memcpy(pseudo + 4, pkt + 16, 4);/* dst_ip */
        pseudo[8] = 0; pseudo[9] = 6;   /* zero + protocol */
        pseudo[10] = 0; pseudo[11] = 20;/* tcp_len */
        uint32_t csum = 0;
        for (int i = 0; i < 12; i += 2)
            csum += ((uint32_t)pseudo[i] << 8) | pseudo[i+1];
        for (int i = 0; i < 20; i += 2)
            csum += ((uint32_t)pkt[20+i] << 8) | pkt[21+i];
        while (csum >> 16) csum = (csum & 0xffff) + (csum >> 16);
        uint16_t tcp_ck = (uint16_t)~csum;
        pkt[36] = (uint8_t)(tcp_ck >> 8);
        pkt[37] = (uint8_t)(tcp_ck & 0xff);

        write_seed("corpus/ip/tcp_syn.bin", pkt, sizeof(pkt));
    }
}

/* ============================================================
 * ICMP ペイロードシード
 * ============================================================ */
static void gen_icmp_seeds(void) {
    printf("[icmp]\n");

    /* シード1: ICMP Echo Request (8バイト最小) */
    {
        uint8_t pkt[8];
        memset(pkt, 0, sizeof(pkt));
        pkt[0] = 8; pkt[1] = 0;         /* type: ECHO REQUEST, code: 0 */
        pkt[2] = 0; pkt[3] = 0;
        pkt[4] = 0; pkt[5] = 1;         /* id */
        pkt[6] = 0; pkt[7] = 1;         /* seq */
        uint16_t ck = cksum(pkt, 8);
        pkt[2] = (uint8_t)(ck >> 8); pkt[3] = (uint8_t)(ck & 0xff);
        write_seed("corpus/icmp/echo_request_min.bin", pkt, sizeof(pkt));
    }

    /* シード2: ICMP Echo Request (データ付き) */
    {
        uint8_t pkt[24];
        memset(pkt, 0, sizeof(pkt));
        pkt[0] = 8; pkt[1] = 0;
        pkt[2] = 0; pkt[3] = 0;
        pkt[4] = 0; pkt[5] = 2;
        pkt[6] = 0; pkt[7] = 3;
        memcpy(pkt + 8, "FUZZTEST12345678", 16);
        uint16_t ck = cksum(pkt, 24);
        pkt[2] = (uint8_t)(ck >> 8); pkt[3] = (uint8_t)(ck & 0xff);
        write_seed("corpus/icmp/echo_request_data.bin", pkt, sizeof(pkt));
    }
}

/* ============================================================
 * TCP セグメントシード
 * ============================================================ */
static void write_tcp_seed(const char *path, uint8_t flags,
                           uint32_t seq, uint32_t ack_seq,
                           const uint8_t *data, size_t data_len) {
    size_t   seg_len = (size_t)20 + data_len;
    uint8_t *seg     = (uint8_t *)calloc(seg_len, 1);
    if (seg == NULL) { perror("calloc"); exit(1); }

    /* TCPヘッダ */
    seg[0] = 0xc0; seg[1] = 0x00;   /* src_port: 49152 */
    seg[2] = 0x1f; seg[3] = 0x90;   /* dst_port: 8080 */
    seg[4]  = (uint8_t)(seq >> 24); seg[5]  = (uint8_t)(seq >> 16);
    seg[6]  = (uint8_t)(seq >> 8);  seg[7]  = (uint8_t)(seq & 0xff);
    seg[8]  = (uint8_t)(ack_seq >> 24); seg[9]  = (uint8_t)(ack_seq >> 16);
    seg[10] = (uint8_t)(ack_seq >> 8);  seg[11] = (uint8_t)(ack_seq & 0xff);
    seg[12] = 0x50;    /* data_off: 5 */
    seg[13] = flags;
    seg[14] = 0x10; seg[15] = 0x00; /* window: 4096 */
    seg[16] = 0; seg[17] = 0;       /* checksum (fill below) */
    seg[18] = 0; seg[19] = 0;

    if (data != NULL && data_len > 0) {
        memcpy(seg + 20, data, data_len);
    }

    /* TCP checksum (IP擬似ヘッダ: src=10.0.0.1, dst=10.0.0.2, proto=6) */
    uint8_t pseudo[12] = {
        10, 0, 0, 1,    /* src_ip */
        10, 0, 0, 2,    /* dst_ip */
        0, 6,           /* zero + proto */
        (uint8_t)(seg_len >> 8), (uint8_t)(seg_len & 0xff)
    };
    uint32_t csum = 0;
    for (size_t i = 0; i < 12; i += 2)
        csum += ((uint32_t)pseudo[i] << 8) | pseudo[i+1];
    for (size_t i = 0; i < seg_len; i += 2) {
        if (i + 1 < seg_len)
            csum += ((uint32_t)seg[i] << 8) | seg[i+1];
        else
            csum += (uint32_t)seg[i] << 8;
    }
    while (csum >> 16) csum = (csum & 0xffff) + (csum >> 16);
    uint16_t tcp_ck = (uint16_t)~csum;
    seg[16] = (uint8_t)(tcp_ck >> 8);
    seg[17] = (uint8_t)(tcp_ck & 0xff);

    write_seed(path, seg, seg_len);
    free(seg);
}

static void gen_tcp_seeds(void) {
    printf("[tcp]\n");

    /* シード1: SYN */
    write_tcp_seed("corpus/tcp/syn.bin", 0x02, 1000, 0, NULL, 0);

    /* シード2: ACK (3ウェイハンドシェイク完了) */
    write_tcp_seed("corpus/tcp/ack.bin", 0x10, 1001, 2001, NULL, 0);

    /* シード3: PSH+ACK (データ送信) */
    write_tcp_seed("corpus/tcp/psh_ack.bin", 0x18, 1001, 2001,
                   (const uint8_t *)"Hello, TCP Stack!", 18);

    /* シード4: FIN+ACK (接続終了) */
    write_tcp_seed("corpus/tcp/fin_ack.bin", 0x11, 1019, 2001, NULL, 0);

    /* シード5: RST */
    write_tcp_seed("corpus/tcp/rst.bin", 0x04, 1001, 2001, NULL, 0);
}

/* ============================================================
 * メイン
 * ============================================================ */
int main(void) {
    printf("Generating fuzz seed corpus...\n\n");

    gen_eth_seeds();
    printf("\n");
    gen_arp_seeds();
    printf("\n");
    gen_ip_seeds();
    printf("\n");
    gen_icmp_seeds();
    printf("\n");
    gen_tcp_seeds();
    printf("\nDone.\n");
    return 0;
}
