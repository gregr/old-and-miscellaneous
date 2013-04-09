#include "util.h"

////////////////////////////////////////////////////////////////
// QAlloc

void QALLOC_free(QAlloc* al) {
    ARRAY_foreach(void*, *al, free(*x));
    ARRAY_resizeWith(void*, *al, 0, 0);
}

////////////////////////////////////////////////////////////////
// Cons

Cons* cons(void* h, Cons* t, QAlloc* al) {
    Cons* c;
    QALLOC(c, sizeof(Cons), *al);
    c->hd = h;
    c->tl = t;
    return c;
}

void Cons_foreach(void(*f)(void*), Cons* c) {
    while (c != 0) { (*f)(c->hd); c = c->tl; }
}
