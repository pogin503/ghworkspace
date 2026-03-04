/*
 * test_utils.c - RFC 1071 チェックサム関数のユニットテスト
 *
 * テスト対象: src/utils.c
 *   - checksum_partial()
 *   - checksum_finish()
 *   - checksum()
 */
#include "unity.h"
#include "utils.h"

#include <stdint.h>
#include <string.h>

void setUp(void) {}
void tearDown(void) {}

/* ゼロ埋めデータのチェックサム: ~0 = 0xffff */
void test_checksum_all_zeros(void) {
    uint8_t data[4] = {0x00, 0x00, 0x00, 0x00};
    TEST_ASSERT_EQUAL_HEX16(0xffff, checksum(data, 4));
}

/* 既知値: 0x0001 + 0x0002 = 0x0003 → ~0x0003 = 0xfffc */
void test_checksum_known_value(void) {
    uint8_t data[4] = {0x00, 0x01, 0x00, 0x02};
    TEST_ASSERT_EQUAL_HEX16(0xfffc, checksum(data, 4));
}

/* 2バッファに分割した partial チェーン計算 = 一括計算と同一 */
void test_checksum_partial_chain(void) {
    uint8_t a[2] = {0x00, 0x01};
    uint8_t b[2] = {0x00, 0x02};

    /* 分割計算 */
    uint32_t csum = checksum_partial(a, 2, 0);
    csum          = checksum_partial(b, 2, csum);
    uint16_t result_chain = checksum_finish(csum);

    /* 一括計算 */
    uint8_t full[4] = {0x00, 0x01, 0x00, 0x02};
    uint16_t result_full = checksum(full, 4);

    TEST_ASSERT_EQUAL_HEX16(result_full, result_chain);
}

/*
 * 奇数バイト長: 末尾を上位バイトとして扱う (RFC 1071)
 * {0x00, 0x01, 0x02} → 0x0001 + 0x0200 = 0x0201 → ~0x0201 = 0xfdfe
 */
void test_checksum_odd_length(void) {
    uint8_t data[3] = {0x00, 0x01, 0x02};
    TEST_ASSERT_EQUAL_HEX16(0xfdfe, checksum(data, 3));
}

/*
 * チェックサム検証: データにチェックサムを埋め込んで全体を計算すると 0x0000
 * RFC 1071: "The receiver checksums the entire block; if the result is 0xffff, OK"
 */
void test_checksum_verify(void) {
    uint8_t data[4] = {0x00, 0x01, 0x00, 0x02};
    uint16_t csum = checksum(data, 4);  /* 0xfffc */

    /* チェックサムをビッグエンディアンで末尾に付加 */
    uint8_t full[6];
    memcpy(full, data, 4);
    full[4] = (uint8_t)(csum >> 8);
    full[5] = (uint8_t)(csum & 0xff);

    TEST_ASSERT_EQUAL_HEX16(0x0000, checksum(full, 6));
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_checksum_all_zeros);
    RUN_TEST(test_checksum_known_value);
    RUN_TEST(test_checksum_partial_chain);
    RUN_TEST(test_checksum_odd_length);
    RUN_TEST(test_checksum_verify);
    return UNITY_END();
}
