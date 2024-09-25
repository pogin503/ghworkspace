#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

// struct sockaddr_in {
//     sa_family_t    sin_family; /* address family: AF_INET */
//     in_port_t      sin_port;   /* port in network byte order */
//     struct in_addr sin_addr;   /* internet address */
// };
// /* Internet address. */
// struct in_addr {
//     uint32_t       s_addr;     /* address in network byte order */
// };

int main() {
    int sock1, sock2;
    struct sockaddr_in addr1, addr2;
    struct pollfd fds[2];
    char buf[2048];
    // int timeout_msecs = 10000; // タイムアウト時間（ミリ秒）

    /* 受信ソケットを2つ作ります */
    sock1 = socket(AF_INET, SOCK_DGRAM, 0);
    sock2 = socket(AF_INET, SOCK_DGRAM, 0);

    addr1.sin_family = AF_INET;
    addr2.sin_family = AF_INET;

    // #include <arpa/inet.h>
    // int inet_pton(int af, const char *src, void *dst);
    inet_pton(AF_INET, "127.0.0.1", &addr1.sin_addr.s_addr);
    inet_pton(AF_INET, "127.0.0.1", &addr2.sin_addr.s_addr);

    // ファイルディスクリプタの設定
    fds[0].fd = sock1;
    fds[0].events = POLLIN | POLLERR; // 読み込みイベントを監視

    fds[1].fd = sock2;
    fds[1].events = POLLIN | POLLERR; // 読み込みイベントを監視

  /*
  無限ループです
  このサンプルでは、この無限ループを抜けません
  */
  while (1) {

    /* fdsに設定されたソケットが読み込み可能になるまで待ちます */
    poll(fds, 2, -1);

    /* sock1に読み込み可能データがある場合 */
    if (fds[0].revents & POLLIN) {
      /* sock1からデータを受信して表示します */
      memset(buf, 0, sizeof(buf));
      recv(sock1, buf, sizeof(buf), 0);
      printf("%s\n", buf);
    }

    /* sock2に読み込み可能データがある場合 */
    if (fds[1].revents & POLLIN) {
      /* sock2からデータを受信して表示します */
      memset(buf, 0, sizeof(buf));
      recv(sock2, buf, sizeof(buf), 0);
      printf("%s\n", buf);
    }
  }

  /* このサンプルでは、ここへは到達しません */
  close(sock1);
  close(sock2);

  return 0;
}
