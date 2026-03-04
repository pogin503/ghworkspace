#include <stdio.h>
#include <stdint.h> // intptr_t
#include <inttypes.h> // PRIdPTR

int main() {
    int x = 10;

    // ポインタ変数
    int *p;

    // xのアドレスを代入
    p = &x;

    // pの参照先のxの値を書き換える
    *p = 15;

    intptr_t ptrVal = (intptr_t)p;

    printf("x = %d\n", x);
    // %pでアドレスを16進数で表示
    printf("&x = %p\n", &x);
    printf("p = %p\n", p);
    // PRI<型><型>
    printf("ptrVal = %" PRIdPTR "\n", ptrVal);
    printf("*p = %d\n", *p);
    printf("&p = %p\n", &p);

    // intptr_t をポインタに戻す
    int *p2 = (int *)ptrVal;
    printf("p2 = %d\n", *p2);
}