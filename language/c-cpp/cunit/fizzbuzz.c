#include <stdio.h>

void fizzbuzz(int num, char *result) {
    if ((num % 3 == 0) && (num % 5 == 0)) {
        sprintf(result, "FizzBuzz");
    } else if (num % 3 == 0) {
        sprintf(result, "Fizz");
    } else if (num % 5 == 0) {
        sprintf(result, "Buzz");
    } else {
        sprintf(result, "%d", num);
    }
}
