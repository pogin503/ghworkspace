#ifndef TUN_H
#define TUN_H

#include <stdint.h>
#include <stddef.h>
#include <sys/types.h>

#define TUN_DEV_PATH  "/dev/net/tun"
#define TUN_MTU       1500
/* Ethernetヘッダ(14バイト)分の余裕を持たせたバッファサイズ */
#define TUN_BUF_SIZE  (TUN_MTU + 14 + 4)

/*
 * ネットワークデバイスとスタック自身のアイデンティティを表す構造体。
 * 全モジュールがフレーム送信のためにポインタとして受け取る。
 */
struct netdev {
    int      fd;          /* /dev/net/tun のファイルディスクリプタ */
    char     name[16];    /* インターフェース名 (例: "tap0") */
    uint8_t  mac[6];      /* スタックのMACアドレス */
    uint32_t ip;          /* スタックのIPv4アドレス (ネットワークバイトオーダー) */
    uint32_t netmask;     /* ネットワークバイトオーダー */
};

/*
 * TAPデバイスをオープンして設定する。
 * dev_name: "tap0" などのインターフェース名、または "" でカーネルに任せる。
 * 成功時0、失敗時-1を返す。
 */
int     tun_alloc(struct netdev *dev, const char *dev_name);

/* TAPデバイスからEthernetフレームを1つ読み込む。読込バイト数または-1を返す */
ssize_t tun_read(struct netdev *dev, uint8_t *buf, size_t buf_len);

/* TAPデバイスへEthernetフレームを1つ書き込む。書込バイト数または-1を返す */
ssize_t tun_write(struct netdev *dev, const uint8_t *buf, size_t len);

/* TAPのファイルディスクリプタを閉じる */
void    tun_close(struct netdev *dev);

#endif /* TUN_H */
