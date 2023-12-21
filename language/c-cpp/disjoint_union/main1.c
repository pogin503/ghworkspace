#include <stdio.h>

void printb(unsigned int v) {
    unsigned int mask = (int)1 << (sizeof(v) * CHAR_BIT - 1)
    do putchar(mask & v ? '1' : '0');
    while (mask >>= 1);
}
int main() {
    int N = 4;
    printf("%d\n", N);
    printf("%d, %b\n", 1 << 4);
}
