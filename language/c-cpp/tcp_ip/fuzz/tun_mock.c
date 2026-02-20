/*
 * tun_mock.c - TUN/TAPデバイス関数のモック実装
 *
 * ファズテスト時には実際の /dev/net/tun を使わず、
 * 送信データを単純に破棄し、読み込みは何も返さない。
 */
#include "tun.h"

#include <string.h>

/*
 * フレームを書き込む: ファズ時は何もせずlenを返す。
 * ip_send → eth_send → tun_write の経路で呼ばれる。
 */
ssize_t tun_write(struct netdev *dev, const uint8_t *buf, size_t len) {
    (void)dev;
    (void)buf;
    return (ssize_t)len;
}

/*
 * フレームを読み込む: ファズターゲットは直接パーサーを呼ぶため不要。
 */
ssize_t tun_read(struct netdev *dev, uint8_t *buf, size_t buf_len) {
    (void)dev;
    (void)buf;
    (void)buf_len;
    return -1;
}

/* デバイスオープン: ファズ時はfdを固定値にして成功を返す */
int tun_alloc(struct netdev *dev, const char *dev_name) {
    (void)dev_name;
    dev->fd = 3;  /* 有効に見えるfd (実際には使用しない) */
    return 0;
}

void tun_close(struct netdev *dev) {
    dev->fd = -1;
}
