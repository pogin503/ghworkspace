#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/** 
 * 文字列のjoin
 */
char * join_strings(const char **strings, size_t count) {
    size_t totalLength = 1;
    for (size_t i = 0; i < count; i++) {
        totalLength += strlen(strings[i]) + (i < (count - 1) ? 1 : 0);
    }

    // 結果となる文字列用のメモリを確保
    char *result = malloc(totalLength);
    if (result == NULL) {
        // メモリ確保失敗
        return NULL;
    }
    result[0] = '\0';

    // 文字列を結合
    for (size_t i = 0; i < count; i++) {
        strcat(result, strings[i]);
        if (i < (count - 1)) {
            strcat(result, ",");
        }
    }

    return result;
}

int main() {
    char buf[256] = { '\0' };
    printf("文字列の入力: ");
    // &bufではない
    scanf("%s", buf);
    
    printf("文字列の出力: %s\n", buf);
    
    // 文字列定義(静的)
    // サイズをピッタリで定義する場合 `文字列の長さ + 1`で配列を定義する
    // 変更しない文字列にはconstをつける
    const char hello[6] = "Hello";
    const char world[7] = "world!";
    
    printf("%s, %s\n", hello, world);
    
    // 複数個の文字列(静的)
    const char abcde[][3] = { "ab", "cd", "ef" };
    
    printf("複数の文字列の出力: \n");
    for (int i = 0; i < 3; i++) {
        printf("%s\n", abcde[i]);
    }
    
    // 文字列(動的配列)
    
    // 2次元の配列(動的)
    int a[] = {1, 2, 3};
    // 配列の長さの算出
    size_t a_size = sizeof(a) / sizeof(a[0]);
    char *strArray[a_size];
    for (size_t i = 0; i < a_size; i++) {
        char *str = malloc(20);
        sprintf(str, "[%d]", a[i]);
        strArray[i] = str;
    }
    
    char *joinedString = join_strings((const char **)strArray, a_size);
    
    if (joinedString != NULL) {
        printf("[%s]\n", joinedString);
        free(joinedString);
    }
    
    // 文字列→数値
    char str[1000];
    char *e;
    scanf("%s", str);
    const int s1 = (int)strtol(str, &e, 10);
    printf("%d", s1);
}