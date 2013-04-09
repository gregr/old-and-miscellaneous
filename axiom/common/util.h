#ifndef axiom_common_util_H_
#define axiom_common_util_H_

#include "array.h"
#include "mem.h"

#define QALLOC(lhs, sz, al)                                             \
    do { MALLOC(lhs, sz); ARRAY_push(void*, al, (void*)lhs); } while (0)

ARRAY_STRUCT(void*, QAlloc);
void QALLOC_free(QAlloc*);

typedef struct Cons_ {
    void* hd;
    struct Cons_* tl;
} Cons;

Cons* cons(void*, Cons*, QAlloc*);
#define CONS(h, t, a) cons((void*)h, t, a)
void Cons_foreach(void(*f)(void*), Cons*);

#endif
