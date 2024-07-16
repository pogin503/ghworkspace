#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#define BUFFER_SIZE 100

// ファイルにデータを書き込む関数
int write_to_file(const char *filename, const char *data) {
    struct stat lstat_buf;
    if (lstat(filename, &lstat_buf) == -1) {
        perror("lstat失敗");
        return EXIT_FAILURE;
    }

    FILE *fp = fopen(filename, "w");
    if (fp == NULL) {
        perror("ファイルを開くことができませんでした");
        return EXIT_FAILURE;
    }

    int fd = fileno(fp);
    struct stat fstat_buf;
    if (fstat(fd, &fstat_buf) == -1) {
        perror("fstat失敗");
        fclose(fp);
        return EXIT_FAILURE;
    }

    if (lstat_buf.st_ino != fstat_buf.st_ino || lstat_buf.st_dev != fstat_buf.st_dev) {
        fprintf(stderr, "ファイルが変更されました\n");
        fclose(fp);
        return EXIT_FAILURE;
    }

    if (fprintf(fp, "%s", data) < 0) {
        perror("ファイルへの書き込みに失敗しました");
        fclose(fp);
        return EXIT_FAILURE;
    }

    if (fclose(fp) != 0) {
        perror("ファイルを閉じることができませんでした");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

// ファイルからデータを読み取る関数
int read_from_file(const char *filename, char *buffer, size_t size) {
    struct stat lstat_buf;
    if (lstat(filename, &lstat_buf) == -1) {
        perror("lstat失敗");
        return EXIT_FAILURE;
    }

    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        perror("ファイルを開くことができませんでした");
        return EXIT_FAILURE;
    }

    int fd = fileno(fp);
    struct stat fstat_buf;
    if (fstat(fd, &fstat_buf) == -1) {
        perror("fstat失敗");
        fclose(fp);
        return EXIT_FAILURE;
    }

    if (lstat_buf.st_ino != fstat_buf.st_ino || lstat_buf.st_dev != fstat_buf.st_dev) {
        fprintf(stderr, "ファイルが変更されました\n");
        fclose(fp);
        return EXIT_FAILURE;
    }

    size_t total_read = 0;
    while (fgets(buffer + total_read, size - total_read, fp) != NULL) {
        total_read += strlen(buffer + total_read);
        if (total_read >= size - 1) {
            break;
        }
    }

    if (fclose(fp) != 0) {
        perror("ファイルを閉じることができませんでした");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

int main() {
    const char *filename = "example.txt";
    const char *data = "Hello, World!\nThis is a sample file.\n";
    char buffer[BUFFER_SIZE] = {0};

    if (write_to_file(filename, data) == EXIT_FAILURE) {
        fprintf(stderr, "ファイルへの書き込みに失敗しました\n");
        return EXIT_FAILURE;
    }

    if (read_from_file(filename, buffer, BUFFER_SIZE) == EXIT_FAILURE) {
        fprintf(stderr, "ファイルの読み取りに失敗しました\n");
        return EXIT_FAILURE;
    }

    printf("ファイルの内容:\n%s", buffer);

    return EXIT_SUCCESS;
}

