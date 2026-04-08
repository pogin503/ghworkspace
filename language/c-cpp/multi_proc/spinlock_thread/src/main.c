/**
 * @file main.c
 * @brief pthread_spinlock_t を使ったスレッド間スピンロックのサンプル
 *
 * 複数スレッドが共有カウンタをインクリメントする。
 * spinlock で保護することで競合状態を防ぎ、
 * Expected と Got が一致することを確認する。
 */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_THREADS 4

static pthread_spinlock_t g_lock;
static long g_counter = 0;

/**
 * @brief 各スレッドの start routine
 *
 * spinlock で保護しながら共有カウンタを iterations 回インクリメントする。
 * @param arg int* にキャストして iterations の値を受け取る
 */
static void *worker(void *arg)
{
    int iterations = *(int *)arg;
    for (int i = 0; i < iterations; i++) {
        pthread_spin_lock(&g_lock);
        g_counter++;
        pthread_spin_unlock(&g_lock);
    }
    return NULL;
}

int main(void)
{
    pthread_t threads[NUM_THREADS];
    int iterations = 100000;

    int ret;
    if ((ret = pthread_spin_init(&g_lock, PTHREAD_PROCESS_PRIVATE)) != 0) {
        fprintf(stderr, "pthread_spin_init: %s\n", strerror(ret));
        return EXIT_FAILURE;
    }

    for (int i = 0; i < NUM_THREADS; i++) {
        if ((ret = pthread_create(&threads[i], NULL, worker, &iterations)) != 0) {
            fprintf(stderr, "pthread_create: %s\n", strerror(ret));
            return EXIT_FAILURE;
        }
    }

    for (int i = 0; i < NUM_THREADS; i++) {
        pthread_join(threads[i], NULL);
    }

    printf("Expected: %d, Got: %ld\n", NUM_THREADS * iterations, g_counter);

    pthread_spin_destroy(&g_lock);
    return EXIT_SUCCESS;
}
