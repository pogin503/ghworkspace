/*
 * test_arp.c - ARP キャッシュ操作のユニットテスト
 *
 * テスト対象: src/arp.c
 *   - arp_cache_update()
 *   - arp_cache_lookup()
 *
 * ARPキャッシュは静的変数のためプロセス内で共有される。
 * テスト間の衝突を避けるため、各テストで異なるIPアドレスを使用する。
 */
#include "unity.h"
#include "arp.h"

#include <stdint.h>
#include <string.h>

void setUp(void) {}
void tearDown(void) {}

static const uint8_t MAC_A[6] = {0x00, 0x11, 0x22, 0x33, 0x44, 0x55};
static const uint8_t MAC_B[6] = {0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF};

/* 未登録IPを検索すると NULL を返す */
void test_arp_cache_lookup_unknown(void) {
    /* このIPは他のテストで使わない値 */
    uint32_t ip = 0xC0A8FF01u; /* 192.168.255.1 */
    TEST_ASSERT_NULL(arp_cache_lookup(ip));
}

/* 登録したエントリを検索で取得できる */
void test_arp_cache_update_and_lookup(void) {
    uint32_t ip = 0x0a000101u; /* 10.0.1.1 */
    arp_cache_update(ip, MAC_A);

    const uint8_t *found = arp_cache_lookup(ip);
    TEST_ASSERT_NOT_NULL(found);
    TEST_ASSERT_EQUAL_MEMORY(MAC_A, found, 6);
}

/* 同一IPを再登録するとMACが上書きされる */
void test_arp_cache_update_overwrite(void) {
    uint32_t ip = 0x0a000102u; /* 10.0.1.2 */
    arp_cache_update(ip, MAC_A);
    arp_cache_update(ip, MAC_B);

    const uint8_t *found = arp_cache_lookup(ip);
    TEST_ASSERT_NOT_NULL(found);
    TEST_ASSERT_EQUAL_MEMORY(MAC_B, found, 6);
}

/* 複数のエントリを登録しても互いに干渉しない */
void test_arp_cache_multiple_entries(void) {
    uint32_t ip1 = 0x0a000201u; /* 10.0.2.1 */
    uint32_t ip2 = 0x0a000202u; /* 10.0.2.2 */

    arp_cache_update(ip1, MAC_A);
    arp_cache_update(ip2, MAC_B);

    const uint8_t *found1 = arp_cache_lookup(ip1);
    const uint8_t *found2 = arp_cache_lookup(ip2);

    TEST_ASSERT_NOT_NULL(found1);
    TEST_ASSERT_NOT_NULL(found2);
    TEST_ASSERT_EQUAL_MEMORY(MAC_A, found1, 6);
    TEST_ASSERT_EQUAL_MEMORY(MAC_B, found2, 6);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_arp_cache_lookup_unknown);
    RUN_TEST(test_arp_cache_update_and_lookup);
    RUN_TEST(test_arp_cache_update_overwrite);
    RUN_TEST(test_arp_cache_multiple_entries);
    return UNITY_END();
}
