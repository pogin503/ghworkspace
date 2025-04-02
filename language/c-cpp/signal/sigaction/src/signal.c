#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

volatile sig_atomic_t sigint_received = 0;

// SIGINT シグナルを捕捉するためのハンドラ関数
void sigint_handler(int __attribute__((unused)) sig) {
    // シグナルを受け取ったことを示すフラグを設定
    sigint_received = 1;
}

int main() {
    // SIGINT シグナルハンドラを設定
    signal(SIGINT, sigint_handler);

    printf("Ctrl+C を押して SIGINT シグナルを送信してください。\n");

    // 無限ループでプログラムを実行し続ける
    while (!sigint_received) {
        // 忙しくないループのために短いスリープを挿入
        sleep(1);
    }

    // シグナルを受け取った後の処理
    printf("SIGINT シグナルを受け取りました。\n");
    printf("プログラムを終了します。\n");

    return 0;
}