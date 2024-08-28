#include <stdio.h>
#include <stddef.h>

// #define offsetof(type, member) (char*)&((type*)0)->member
#define alignmentof(type) offsetof(struct { char a; type b; }, b)


struct S1 {
  short s;
  unsigned char uc[3];
};

struct S2 {
  char string[10];
  unsigned char uc;
};

struct S3 {
  double d;
  long long ll;
  char c;
};

struct S4 {
  int a;    // サイズ，アラインメントとも４バイトとする．
  char b;   // サイズ，アラインメントとも１バイト (Ｃの仕様)．
};

// sizeof(共用体) で説明した例
typedef union U {
  char string[17];
  double d[2];
} U_t;

typedef struct {
    char c;
    short s;
    int i;
    long l;
    long long ll;
    float f;
    double d;
} test;

#define AlignmentOf(type)    offsetof(struct { char a; type b; }, b)
static const char format[] = "%12s sizeof(%s)=%zu, __alignof__(%s)=%zu, AlignmentOf(%s)=%zu.\n";

#define PrintSizeAndAlignment(type) \
  printf(format, #type, #type, sizeof(type), #type, __alignof__(type), #type, AlignmentOf(type))

int main(void){
    // test t = {1, 2, 3, 4, 5, 6, 7};

    printf("******** SHOW PRIMITIVE DATA SIZE ********\n");
    PrintSizeAndAlignment(char);
    // PrintSizeAndAlignment(unsigned char);
    PrintSizeAndAlignment(short);
    // PrintSizeAndAlignment(unsigned short);
    PrintSizeAndAlignment(int);
    // PrintSizeAndAlignment(unsigned int);
    PrintSizeAndAlignment(long);
    // PrintSizeAndAlignment(unsigned long);
    PrintSizeAndAlignment(long long);
    // PrintSizeAndAlignment(unsigned long long);
    PrintSizeAndAlignment(float);
    PrintSizeAndAlignment(double);
    PrintSizeAndAlignment(long double);
    PrintSizeAndAlignment(void *);
    PrintSizeAndAlignment(struct S1);
    PrintSizeAndAlignment(struct S2);
    PrintSizeAndAlignment(struct S3);
    PrintSizeAndAlignment(struct S4);
    PrintSizeAndAlignment(U_t);
    PrintSizeAndAlignment(test);
    // printf("char      | %9zd | %zd\n", sizeof(char), __alignof__(char));
    // printf("short     | %9zd | %zd\n", sizeof(short), __alignof__(short));
    // printf("int       | %9zd | %zd\n", sizeof(int), __alignof__(int));
    // printf("long      | %9zd | %zd\n", sizeof(long), __alignof__(long));
    // printf("long long | %9zd | %zd\n", sizeof(long long), __alignof__(long long));
    // printf("float     | %9zd | %zd\n", sizeof(float), __alignof__(float));
    // printf("double    | %9zd | %zd\n", sizeof(double), __alignof__(double));


    printf("offsetof : c = %zu, s = %zu, i = %zu, l = %zu, ll = %zu, f = %zu, d = %zu\n", \
           offsetof(test, c), offsetof(test, s), offsetof(test, i), offsetof(test, l), offsetof(test, ll), offsetof(test, f), offsetof(test, d));
    printf("alignmentof(test) = %zu\n", alignmentof(test));

    return 0;
}
