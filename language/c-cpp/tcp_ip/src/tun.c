#include "tun.h"
#include "utils.h"

#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <linux/if.h>
#include <linux/if_tun.h>

/*
 * TAPデバイスをオープンする。
 * IFF_TAP: Layer 2 (Ethernet) フレームを扱う
 * IFF_NO_PI: struct tun_pi プレフィックスなしでrawフレームを読み書きする
 */
int tun_alloc(struct netdev *dev, const char *dev_name) {
    struct ifreq ifr;
    int fd;
    int err;

    fd = open(TUN_DEV_PATH, O_RDWR);
    if (fd < 0) {
        log_err("open(%s): %s", TUN_DEV_PATH, strerror(errno));
        return -1;
    }

    memset(&ifr, 0, sizeof(ifr));
    ifr.ifr_flags = IFF_TAP | IFF_NO_PI;

    if (dev_name != NULL && dev_name[0] != '\0') {
        strncpy(ifr.ifr_name, dev_name, IFNAMSIZ - 1);
    }

    err = ioctl(fd, TUNSETIFF, (void *)&ifr);
    if (err < 0) {
        log_err("ioctl(TUNSETIFF): %s", strerror(errno));
        close(fd);
        return -1;
    }

    dev->fd = fd;
    strncpy(dev->name, ifr.ifr_name, sizeof(dev->name) - 1);
    dev->name[sizeof(dev->name) - 1] = '\0';

    log_info("TAP device '%s' opened (fd=%d)", dev->name, dev->fd);
    return 0;
}

/* TAPデバイスからフレームを読み込む (IFF_NO_PI設定のためrawフレームが届く) */
ssize_t tun_read(struct netdev *dev, uint8_t *buf, size_t buf_len) {
    ssize_t n = read(dev->fd, buf, buf_len);
    if (n < 0) {
        log_err("tun_read: %s", strerror(errno));
    }
    return n;
}

/* TAPデバイスへrawフレームを書き込む */
ssize_t tun_write(struct netdev *dev, const uint8_t *buf, size_t len) {
    ssize_t n = write(dev->fd, buf, len);
    if (n < 0) {
        log_err("tun_write: %s", strerror(errno));
    }
    return n;
}

void tun_close(struct netdev *dev) {
    if (dev->fd >= 0) {
        close(dev->fd);
        dev->fd = -1;
    }
}
