#include <stdio.h>
#include <stdlib.h>

#define PRINTARRAY1D(array, length) \
    printf(#array "[] = { "); \
    for(int (i) = 0; i < length - 1; i++) \
        printf("%d,", array[i]); \
    printf("%d }\n", array[length - 1])

/// 1. 配列はreturnで返せないので引数で返す
/// 2. 配列のサイズは、配列のポインタに対してsizeofで計算すると期待した
///    計算結果が返ってこない。関数内で計算せず、仮引数として渡すことが必要。
void calc_square(int *in, int in_size, int *out) {
    for (int i = 0; i < in_size; i++) {
        out[i] = in[i] * in[i];
    }
}

int main() {
    // 定数の配列の定義
    int a[4] = {1, 2, 3, 4};
    
    // 配列の長さ取得
    int a_size = sizeof(a) / sizeof(a[0]);
    
    // 1次元配列の処理
    printf("a[] = { ");
    for (int i = 0; i < a_size - 1; i++) {
       printf("%d,", a[i]); 
    }
    printf("%d }\n", a[a_size - 1]);
    printf("a_size = %d\n", a_size);
    
    // 1次元の動的配列を作る
    int n1;
    printf("n1 = ");
    scanf("%d", &n1);
    int *b = (int *)malloc(sizeof(int) * n1);
    if (b == NULL) exit(0);
    for (int i = 0; i < n1; i++) {
        b[i] = i;
    }
    printf("b[] = { ");
    for (int i = 0; i < n1 - 1; i++) {
        printf("%d,", b[i]);
    }
    printf("%d }\n", b[n1 - 1]);
    
    // 1次元の動的配列の領域開放
    free(b);
    
    // 2次元の動的配列を作る(int)
    int row = 3;
    int col = 2;
    int **c = (int **)malloc(sizeof(int *) * row);
    for (int i = 0; i < row; i++) {
        c[i] = (int *)malloc(sizeof(int) * col);
    }
    
    // 2次元の動的配列の領域開放
    for (int i = 0; i < row; i++)
        free(c[i]);
    free(c);
    
    // 関数で結果を返す場合
    int in_arr[4] = {1, 2, 3, 4};
    int in_size = sizeof(in_arr) / sizeof(in_arr[0]);
    int *out_arr = (int *)malloc(sizeof(int) * in_size);
    calc_square(in_arr, in_size, out_arr);
    printf("out_arr[] = { ");
    for (int i = 0; i < in_size - 1; i++) {
        printf("%d,", out_arr[i]);
    }
    printf("%d }\n", out_arr[in_size - 1]);
    // マクロを使用した出力
    PRINTARRAY1D(out_arr, in_size);
    
    free(out_arr);
    
}