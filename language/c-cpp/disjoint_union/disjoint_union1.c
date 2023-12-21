#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/**
  * 非交和を計算する
  * A ⊔ B = {1, 2, 3} ⊔ {1, 2}
  * = {{1, 0}, {2, 0}, {3, 0}, {1, 1}, {2, 1}}
  */
void disjoint_union1(int *a, size_t a_size, int *b, size_t b_size, int **out) {

    for (unsigned int a_i = 0; a_i < a_size; a_i++) {
        out[a_i][0] = a[a_i];
        out[a_i][1] = 0;
    }
    for (unsigned int b_i = 0; b_i < b_size; b_i++) {
        out[b_i + a_size][0] = b[b_i];
        out[b_i + a_size][1] = 1;
    }
}

char * join_strings(const char **strings, size_t count) {
    // 必要な合計長を計算（カンマとヌル文字の分も加える）
    // ヌル終端文字のために初期値は1にする
    size_t totalLength = 1;
    for (unsigned int i = 0; i < count; i++) {
        totalLength += strlen(strings[i]) + (i < (count - 1) ? 1 : 0);
    }
    
    // 結果となる文字列用のメモリを確保
    char *result = malloc(totalLength);
    if (result == NULL) {
        // メモリ確保失敗
        return NULL;
    }
    // 空の文字列で初期化
    result[0] = '\0';

    // 文字列の結合
    for (unsigned int i = 0; i < count; i++) {
        strcat(result, strings[i]);
        if (i < (count - 1)) {
            strcat(result, ",");
        }
    }

    return result;
}

int main() {
    int a[] = {1, 2, 3};
    int b[] = {1, 2};
    size_t a_size = sizeof(a) / sizeof(a[0]);
    size_t b_size = sizeof(b) / sizeof(b[0]);
    size_t ab_size = a_size + b_size;
    int **arr = (int **)malloc(sizeof(int *) * ab_size);
    for(unsigned int i = 0; i < ab_size; i++) {
        arr[i] = (int *)malloc(sizeof(int) * 2);
    }

    disjoint_union1(a, a_size, b, b_size, arr);

    char *strArray[ab_size];
    for (unsigned int i = 0; i < ab_size; i++) {
        char *str = malloc(20);
        sprintf(str, "[%d,%d]", arr[i][0], arr[i][1]);
        strArray[i] = str;
    }

    char *joinedString = join_strings((const char **)strArray, ab_size);

    if (joinedString != NULL) {
        printf("[%s]\n", joinedString);
        free(joinedString);
    }
}
