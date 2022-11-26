#include "stdio.h"
#define printf_d(x) printf("%s = %d",#x,(x))
#define printf_str(x) printf("%s = %s",#x,(x))

#define FOR(i,a,b) for(int i=(a);i<(b);++i)
#define REP(i,n)  FOR(i,0,n)

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <setjmp.h>

#define HEAPSIZE 100
#define FREESIZE 50
#define STACKSIZE 30000
#define SYMSIZE 256
#define BUFSIZE 100
#define NIL 0
#define T 4

//-------read--------
#define EOL     '\n'
#define TAB     '\t'
#define SPACE   ' '
#define ESCAPE  033
#define NUL     '\0'

typedef enum tag {
	EMP, NUM, SYM, LIS, SUBR, FSUBR, FUNC
} tag;

typedef enum flag {
	FRE, USE
} flag;

struct cell {
	tag tag;
	flag flag;
	char *name;
	union {
		int num;
		int bind;
		int(*subr)();
	} val;
	int car;
	int cdr;
};

typedef struct cell cell;

typedef enum toktype {
	LPAREN, RPAREN, QUOTE, DOT, NUMBER, SYMBOL, OTHER
} toktype;

typedef enum backtrack{
	GO, BACK
} backtrack;

struct token {
	char ch;
	backtrack flag;
	toktype type;
	char buf[BUFSIZE];
} token;

struct token stok = {GO, OTHER};

/*
implicit conversion from enumeration type 'enum toktype' to different enumeration type 'backtrack' (aka 'enum backtrack')
 */
#define GET_CAR(addr)       heap[addr].car
#define GET_CDR(addr)       heap[addr].cdr
#define GET_NUMBDRE(addr)   heap[addr].val.num
#define GET_NAME(addr)      heap[addr].name
//
cell heap[HEAPSIZE];

// pointer
int ep; // environment pointer
int hp; // heap pointer
int sp; // stack pointer
int fc; // free counter
int ap; // arglist pointer

// arg check code
#define NUMLIST_TEST 1
#define SYMBOL_TEST  2
#define NUMBER_TEST  3
#define LIST_TEST    4
#define LEN0_TEST    5
#define LEN1_TEST    6
#define LEN2_TEST    7
#define LEN3_TEST    8
#define LENS1_TEST   9
#define LENS2_TEST   10
#define COND_TEST    11

void initcell() {
	int addr, addr1;

	for(addr = 0; addr < HEAPSIZE; addr++) {
		heap[addr].flag = FRE;
		heap[addr].cdr = addr + 1;
	}
	hp = 0;
	fc = HEAPSIZE;

	ep = makesym("nil");
	assocym(makesym("nil", NIL));
	assocym(makesym("t"), makesym("t"));
	sp = 0;
	ap = 0;
}

int symboltoken(char buf[]){
    int i;
    char c;

    if(isdigit(buf[0]))
        return(0);

    i = 0;
    while((c=buf[i]) != NUL)
        if((isalpha(c)) || (isdigit(c)) || (issymch(c)))
            i++;
        else
            return(0);

    return(1);
}

/* void gettoken(); */

//-------read()--------

void gettoken(void){
    char c;
    int pos;

    if(stok.flag == BACK){
        stok.flag = GO;
        return;
    }

    if(stok.ch == ')'){
        stok.type = RPAREN;
        stok.ch = NUL;
        return;
    }

    if(stok.ch == '('){
        stok.type = LPAREN;
        stok.ch = NUL;
        return;
    }

    c = getchar();
    while((c == SPACE) || (c == EOL) || (c == TAB))
        c=getchar();

    switch(c){
        case '(':   stok.type = LPAREN; break;
        case ')':   stok.type = RPAREN; break;
        case '\'':  stok.type = QUOTE; break;
        case '.':   stok.type = DOT; break;
        default: {
            pos = 0; stok.buf[pos++] = c;
            while(((c=getchar()) != EOL) && (pos < BUFSIZE) &&
                    (c != SPACE) && (c != '(') && (c != ')'))
                stok.buf[pos++] = c;

            stok.buf[pos] = NUL;
            stok.ch = c;
            if(numbertoken(stok.buf)){
                stok.type = NUMBER;
                break;
            }
            if(symboltoken(stok.buf)){
                stok.type = SYMBOL;
                break;
            }
            stok.type = OTHER;
        }
    }
}


/* void gettoken(){ */
/* 	char c; */
/* 	int pos; */
/* 	if (stok.flag == BACK){ */

/* 	} */
/* } */

int main(){

    return 0;
}
