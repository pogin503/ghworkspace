
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

int
main()
{
    int sock0 = socket(AF_INET, SOCK_STREAM, 0);;
    if (sock0 < 0) {
        printf("socket failed\n");
        return 1;
    }

    /* ソケットの設定 */
    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(12345);
    addr.sin_addr.s_addr = INADDR_ANY;
    bind(sock0, (struct sockaddr *)&addr, sizeof(addr));    
    
    /* TCPクライアントからの接続要求を待てる状態にする */
    listen(sock0, 5);
    
    struct sockaddr_in client;
    int len = sizeof(client);
    int sock = accept(sock0, (struct sockaddr *)&client, &len);

    write(sock, "hello", 5);

    close(sock);

    return 0;
}
