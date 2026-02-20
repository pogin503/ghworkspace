#include "tun.h"
#include "ethernet.h"
#include "arp.h"
#include "tcp.h"
#include "utils.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>
#include <arpa/inet.h>

/* スタックのデフォルト設定 */
#define STACK_IP      "10.0.0.2"
#define STACK_NETMASK "255.255.255.0"

/* MACアドレス: 00:0c:29:00:00:01 (VMwareのOUI範囲、教育用として使用) */
static const uint8_t STACK_MAC[6] = { 0x00, 0x0c, 0x29, 0x00, 0x00, 0x01 };

#define LISTEN_PORT 8080

static struct netdev dev;
static volatile int running = 1;

static void sigint_handler(int sig) {
    (void)sig;
    running = 0;
}

/* メインのイベントループ: TAPデバイスからフレームを読み込み続ける */
static void run_event_loop(void) {
    uint8_t buf[TUN_BUF_SIZE];

    log_info("Event loop started (Ctrl+C to stop)");

    while (running) {
        ssize_t n = tun_read(&dev, buf, sizeof(buf));
        if (n < 0) {
            if (!running) break;
            continue;
        }
        eth_recv(&dev, buf, (size_t)n);
    }
}

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [tap-device]\n", prog);
    fprintf(stderr, "  tap-device: TAPデバイス名 (デフォルト: tap0)\n");
}

int main(int argc, char *argv[]) {
    const char *dev_name = "tap0";

    if (argc >= 3) {
        print_usage(argv[0]);
        return 1;
    }
    if (argc == 2) {
        dev_name = argv[1];
    }

    /* TCP初期シーケンス番号のランダム化 */
    srand((unsigned int)time(NULL));

    signal(SIGINT,  sigint_handler);
    signal(SIGTERM, sigint_handler);

    memset(&dev, 0, sizeof(dev));
    dev.fd = -1;

    /* TAPデバイスをオープン (CAP_NET_ADMIN権限が必要) */
    if (tun_alloc(&dev, dev_name) < 0) {
        fprintf(stderr, "Failed to open TAP device '%s'\n"
                        "Hint: run as root or with CAP_NET_ADMIN\n", dev_name);
        return 1;
    }

    /* スタックのMACアドレスを設定 */
    memcpy(dev.mac, STACK_MAC, 6);

    /* スタックのIPアドレスを設定 */
    if (inet_pton(AF_INET, STACK_IP, &dev.ip) != 1) {
        fprintf(stderr, "Invalid IP address: %s\n", STACK_IP);
        tun_close(&dev);
        return 1;
    }
    if (inet_pton(AF_INET, STACK_NETMASK, &dev.netmask) != 1) {
        fprintf(stderr, "Invalid netmask: %s\n", STACK_NETMASK);
        tun_close(&dev);
        return 1;
    }

    /* TCPのリスニングポートを開く */
    if (tcp_listen(dev.ip, LISTEN_PORT) < 0) {
        tun_close(&dev);
        return 1;
    }

    /* 起動情報を表示 */
    log_info("=== Userspace TCP/IP Stack ===");
    log_info("Interface : %s", dev.name);
    log_info("MAC       : %02x:%02x:%02x:%02x:%02x:%02x",
             dev.mac[0], dev.mac[1], dev.mac[2],
             dev.mac[3], dev.mac[4], dev.mac[5]);
    log_info("IP        : %s", STACK_IP);
    log_info("TCP LISTEN: port %d", LISTEN_PORT);

    printf("\n");
    printf("--- Host setup (run as root) ---\n");
    printf("  sudo ip tuntap add dev %s mode tap\n", dev.name);
    printf("  sudo ip addr add 10.0.0.1/24 dev %s\n", dev.name);
    printf("  sudo ip link set %s up\n", dev.name);
    printf("\n");
    printf("--- Test commands ---\n");
    printf("  ping -I %s %s          # ICMP echo test\n", dev.name, STACK_IP);
    printf("  nc %s %d                # TCP connection test\n",
           STACK_IP, LISTEN_PORT);
    printf("  sudo tcpdump -i %s -nn  # packet capture\n", dev.name);
    printf("\n");

    run_event_loop();

    log_info("Shutting down...");
    tun_close(&dev);

    return 0;
}
