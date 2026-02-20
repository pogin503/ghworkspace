#ifndef UTILS_H
#define UTILS_H

#include <stdint.h>
#include <stddef.h>

/*
 * RFC 1071 Internet チェックサム
 * checksum_partial: 部分和を計算して返す (複数バッファを連結する場合に使用)
 * checksum_finish:  部分和を折り畳んで1の補数を返す
 * checksum:         1回の呼び出しで完結する簡便版
 */
uint32_t checksum_partial(const void *data, size_t len, uint32_t init);
uint16_t checksum_finish(uint32_t partial);
uint16_t checksum(const void *data, size_t len);

/* デバッグ用ユーティリティ */
void hex_dump(const char *label, const uint8_t *buf, size_t len);
void log_info(const char *fmt, ...);
void log_err(const char *fmt, ...);

#endif /* UTILS_H */
