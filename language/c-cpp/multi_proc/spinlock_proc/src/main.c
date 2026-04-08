/**
 * @file main.c
 * @brief 共有メモリ + atomic_flag によるプロセス間スピンロックのサンプル
 *
 * fork() で生成した子プロセスが MAP_SHARED な匿名メモリ上の
 * atomic_flag をスピンロックとして使い、共有カウンタを保護する。
 * Expected と Got が一致することで、プロセス間でも正しく同期できることを確認する。
 */
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>

#define NUM_PROC   4

/** プロセス間で共有する構造体 */
typedef struct {
    atomic_flag flag;  /**< スピンロック用フラグ */
    long        counter; /**< 共有カウンタ */
} shared_t;

/**
 * @brief スピンロックを獲得する
 *
 * atomic_flag_test_and_set がfalseを返す(= フラグが解放された)まで
 * CPUをビジーウェイトする。
 */
static void spinlock_acquire(atomic_flag *flag)
{
    while (atomic_flag_test_and_set_explicit(flag, memory_order_acquire)) {
        ; /* spin */
    }
}

/**
 * @brief スピンロックを解放する
 */
static void spinlock_release(atomic_flag *flag)
{
    atomic_flag_clear_explicit(flag, memory_order_release);
}

int main(void)
{
    /* MAP_SHARED | MAP_ANONYMOUS: fork後も親子プロセス間でページを共有する */
    shared_t *sh = mmap(NULL, sizeof(shared_t),
                        PROT_READ | PROT_WRITE,
                        MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (sh == MAP_FAILED) {
        perror("mmap");
        return EXIT_FAILURE;
    }

    atomic_flag_clear(&sh->flag);
    sh->counter = 0;

    int iterations = 100000;

    for (int i = 0; i < NUM_PROC; i++) {
        pid_t pid = fork();
        if (pid < 0) {
            perror("fork");
            return EXIT_FAILURE;
        }
        if (pid == 0) {
            /* 子プロセス: spinlock で保護しながらカウンタをインクリメント */
            for (int j = 0; j < iterations; j++) {
                spinlock_acquire(&sh->flag);
                sh->counter++;
                spinlock_release(&sh->flag);
            }
            _exit(EXIT_SUCCESS);
        }
    }

    /* 全子プロセスの終了を待機 */
    for (int i = 0; i < NUM_PROC; i++) {
        wait(NULL);
    }

    printf("Expected: %d, Got: %ld\n", NUM_PROC * iterations, sh->counter);

    munmap(sh, sizeof(shared_t));
    return EXIT_SUCCESS;
}
