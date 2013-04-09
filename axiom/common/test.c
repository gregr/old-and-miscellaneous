#include "util.h"
#include "error.h"
#include <stdio.h>

void pnum(int v) { printf("num: %d\n", v); }
void pnumv(void* v) { pnum((int)v); }

const char* const catchNo = "no";

ARRAY_STRUCT(void*, VoidPtrArray);
ARRAY_STRUCT(int, IntArray);

int main() {
    QAlloc a;
    ARRAY_init(a);
    Cons* nums = CONS(14, CONS(4, CONS(2, 0, &a), &a), &a);
    Cons_foreach(&pnumv, nums);
    QALLOC_free(&a);
    IntArray b;
    ARRAY_init(b);
    ARRAY_push(int, b, 4);
    ARRAY_push(int, b, 5);
    ARRAY_push(int, b, 6);
    ARRAY_push(int, b, 7);
    ARRAY_foreach(int, b, pnum(*x));
    ARRAY_destroy(b);
    TRYCATCH
        (
         TRYFINALLY(
                    //                    raise(catchNo);
                    ,
                    printf("finally1\n");)
         void *yum;
         MALLOC(yum, 4000000000); // a reasonable request
         TRYFINALLY(
                    RAISE("hello", "i said %s", "hello");
                    ,
                    printf("finally2\n");)
         ,
         CATCH(catchNo, { printf("catch no\n"); })
         CATCHALL({ printf("catch all: "); PRINT_EXC(stdout); printf("\n"); })
         );
    RAISE("hmm...", "");
    printf("done\n");
    return 0;
}
