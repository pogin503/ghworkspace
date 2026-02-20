#include "utils.h"

#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>

/*
 * RFC 1071: 16ビット語の和を計算する。
 * ネットワークバイトオーダー (big-endian) の並びで加算する。
 * 奇数バイトの場合は上位バイト扱いでパディングして加算する。
 */
uint32_t checksum_partial(const void *data, size_t len, uint32_t init) {
    const uint8_t *buf = (const uint8_t *)data;
    uint32_t sum = init;

    while (len > 1) {
        sum += ((uint32_t)buf[0] << 8) | (uint32_t)buf[1];
        buf += 2;
        len -= 2;
    }

    /* 奇数バイトが残った場合: 上位バイト扱いで加算 */
    if (len == 1) {
        sum += (uint32_t)buf[0] << 8;
    }

    return sum;
}

/*
 * 32ビットの部分和を16ビットに折り畳み、1の補数を返す。
 * 検証時: 正しいチェックサムを含む全データに対して呼び出すと0を返す。
 */
uint16_t checksum_finish(uint32_t partial) {
    /* 上位16ビットのキャリーを下位に折り畳む */
    while (partial >> 16) {
        partial = (partial & 0xffff) + (partial >> 16);
    }
    return (uint16_t)~partial;
}

/* データ全体のチェックサムを一度に計算する */
uint16_t checksum(const void *data, size_t len) {
    return checksum_finish(checksum_partial(data, len, 0));
}

/* 16バイト単位で16進数+ASCII表示するデバッグ出力 */
void hex_dump(const char *label, const uint8_t *buf, size_t len) {
    fprintf(stderr, "%s (%zu bytes):\n", label, len);
    for (size_t i = 0; i < len; i += 16) {
        fprintf(stderr, "  %04zx: ", i);
        for (size_t j = 0; j < 16; j++) {
            if (i + j < len) {
                fprintf(stderr, "%02x ", buf[i + j]);
            } else {
                fprintf(stderr, "   ");
            }
        }
        fprintf(stderr, " |");
        for (size_t j = 0; j < 16 && i + j < len; j++) {
            int c = buf[i + j];
            fprintf(stderr, "%c", isprint(c) ? c : '.');
        }
        fprintf(stderr, "|\n");
    }
}

void log_info(const char *fmt, ...) {
    va_list args;
    fprintf(stdout, "[INFO] ");
    va_start(args, fmt);
    vfprintf(stdout, fmt, args);
    va_end(args);
    fprintf(stdout, "\n");
    fflush(stdout);
}

void log_err(const char *fmt, ...) {
    va_list args;
    fprintf(stderr, "[ERR]  ");
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fprintf(stderr, "\n");
    fflush(stderr);
}
