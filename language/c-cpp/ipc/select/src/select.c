#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

int
main(void) {

    /* stdin (fd 0) を監視し、入力があった場合に表示する。*/
    fd_set rfds;
    // ファイルディスクリプタの集合を消去する
    FD_ZERO(&rfds);
    // ファイルディスクリプタの集合への追加
    FD_SET(0, &rfds);

    /* 最大5秒間監視する。*/
    struct timeval tv;
    tv.tv_sec = 5;
    tv.tv_usec = 0;

    /* ここでは1個のファイルディスクリプタ(stdin(0))が読み込み可能かを最大5秒待つ */
    int retval = select(1, &rfds, NULL, NULL, &tv);
    // エラーが起こった場合、この時点でのtvの中身は未定義となるため
    // tv の値を信頼してはならない

    if (retval == -1) {
			  perror("select()");
		} else if (retval) {
        printf("今、データが取得できました。\n");
        /* FD_ISSET(0, &rfds) が true になる。*/
    } else {
        printf("5 秒以内にデータが入力されませんでした。\n");
    }

    return 0;
}
