#include <stdio.h>

#include "fizzbuzz.h"

int main(void) {
    int i;
    char result[256];
    for (i = 1; i < 20; i++) {
        fizzbuzz(i, result);
        printf("%s\n", result);
    }
    return 0;
}
