#include <stdio.h>

int main() {
    // 整数(int)の入出力
    int int_n = 123;
    printf("%d\n", int_n);
    int int_n1;
    scanf("%d", &int_n1);
    printf("%d", int_n1);
    
    // 整数(long)の出力
    long long_n = 234;
    printf("%ld\n", long_n);
    
    // 16進数
    printf("%x\n", 16);  //   10
    printf("%#x\n", 16); // 0x10
    // 8進数
    printf("%o\n", 9);  //  11
    printf("%#o\n", 9); // 011
}