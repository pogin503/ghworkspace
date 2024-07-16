
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

int
main()
{
    /* ソケットの作成 */
    int sock = socket(AF_INET, SOCK_STREAM, 0);

    /* 接続先指定用構造体の準備 */
    struct sockaddr_in server;
    server.sin_family = AF_INET;
    server.sin_port = htons(12345);
    server.sin_addr.s_addr = inet_addr("127.0.0.1");

    /* サーバに接続 */
    connect(sock, (struct sockaddr *)&server, sizeof(server));

    /* サーバからデータを受信 */
    char buf[32];
    memset(buf, 0, sizeof(buf));
    int n = read(sock, buf, sizeof(buf));

    printf("%d, %s\n", n, buf);

    /* socketの終了 */
    close(sock);

    return 0;
}