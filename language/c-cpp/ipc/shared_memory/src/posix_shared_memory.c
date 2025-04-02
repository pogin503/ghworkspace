#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>        
#include <fcntl.h>           
#include <unistd.h>          
#include <string.h>
#include <sys/wait.h>

#define SHARED_MEM_NAME "/posix_shared_mem"
#define SHARED_MEM_SIZE 1024

/**
 * @brief 共有メモリを作成して初期化します。
 * 
 * この関数は、指定された名前とサイズで共有メモリオブジェクトを作成し、
 * ファイルディスクリプタを返します。
 * 
 * @return 共有メモリのファイルディスクリプタ
 */
int create_shared_memory() {
    int fd = shm_open(SHARED_MEM_NAME, O_CREAT | O_RDWR, 0666);
    if (fd == -1) {
        perror("shm_open");
        exit(EXIT_FAILURE);
    }

    if (ftruncate(fd, SHARED_MEM_SIZE) == -1) {
        perror("ftruncate");
        exit(EXIT_FAILURE);
    }

    return fd;
}

/**
 * @brief 共有メモリにデータを書き込みます。
 * 
 * 指定されたファイルディスクリプタに対応する共有メモリにデータを書き込みます。
 * 
 * @param fd 共有メモリのファイルディスクリプタ
 * @param message 書き込むメッセージ
 */
void write_to_shared_memory(int fd, const char *message) {
    void *ptr = mmap(NULL, SHARED_MEM_SIZE, PROT_WRITE, MAP_SHARED, fd, 0);
    if (ptr == MAP_FAILED) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }

    snprintf((char *)ptr, SHARED_MEM_SIZE, "%s", message);

    if (munmap(ptr, SHARED_MEM_SIZE) == -1) {
        perror("munmap");
        exit(EXIT_FAILURE);
    }
}

/**
 * @brief 共有メモリからデータを読み込みます。
 * 
 * 指定されたファイルディスクリプタに対応する共有メモリからデータを読み込み、標準出力に表示します。
 * 
 * @param fd 共有メモリのファイルディスクリプタ
 */
void read_from_shared_memory(int fd) {
    void *ptr = mmap(NULL, SHARED_MEM_SIZE, PROT_READ, MAP_SHARED, fd, 0);
    if (ptr == MAP_FAILED) {
        perror("mmap");
        exit(EXIT_FAILURE);
    }

    printf("Read from shared memory: %s\n", (char *)ptr);

    if (munmap(ptr, SHARED_MEM_SIZE) == -1) {
        perror("munmap");
        exit(EXIT_FAILURE);
    }
}

/**
 * @brief 共有メモリを削除します。
 * 
 * この関数は、指定された名前の共有メモリオブジェクトを削除します。
 */
void delete_shared_memory() {
    if (shm_unlink(SHARED_MEM_NAME) == -1) {
        perror("shm_unlink");
        exit(EXIT_FAILURE);
    }
}

/**
 * @brief プログラムのエントリーポイントです。
 * 
 * この関数は、親プロセスと子プロセス間で共有メモリを使用してメッセージをやり取りします。
 * 
 * @return 正常終了時は0を返します。
 */
int main() {
    int fd = create_shared_memory();

    pid_t pid = fork();
    if (pid == -1) {
        perror("fork");
        exit(EXIT_FAILURE);
    }

    if (pid == 0) {  // 子プロセス
        sleep(1);  // 親プロセスが書き込むまで待つ
        read_from_shared_memory(fd);
    } else {  // 親プロセス
        write_to_shared_memory(fd, "Hello from parent process!");
        wait(NULL);  // 子プロセスの終了を待つ
        delete_shared_memory();
    }

    close(fd);
    return 0;
}

