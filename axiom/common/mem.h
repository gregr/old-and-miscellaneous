#ifndef axiom_common_mem_H_
#define axiom_common_mem_H_

#include "error.h"
#include <stdlib.h>

extern const char* const ET_MEM;
extern int(*mem_failHook)(size_t);

#define ALLOC_(lhs, sz, op, errMsg) do {                        \
        size_t sz_ = sz;                                        \
        if ((lhs = op) != 0) break;                             \
        if (mem_failHook == 0) RAISE(ET_MEM, errMsg, sz_);      \
    } while (mem_failHook(sz));

#define MALLOC(lhs, sz) ALLOC_(lhs, sz, malloc(sz_), "malloc(%u)")

#define REALLOC(lhs, rhs, sz)                           \
    ALLOC_(lhs, sz, realloc(rhs, sz_), "realloc(_, %u)")

#endif
