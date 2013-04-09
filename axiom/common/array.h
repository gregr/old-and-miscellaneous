#ifndef axiom_common_array_H_
#define axiom_common_array_H_

#include "mem.h"
#include "error.h"
#include <string.h>
#include <stddef.h>

#define ARRAY_STRUCT_BARE(ty) struct { ty* begin; ty* end; ty* lim; }
#define ARRAY_STRUCT(ty, name) typedef ARRAY_STRUCT_BARE(ty) name

#define ARRAY_init(a) do { (a).begin = 0; (a).end = 0; (a).lim = 0; } while (0)
#define ARRAY_destroy(a) do { free((a).begin); } while (0)
#define ARRAY_size(a) ((size_t)((a).end - (a).begin))
#define ARRAY_capacity(a) ((a).lim - (a).begin)

#define ARRAY_check(a, i) do { ASSERT(i < ARRAY_size(a)); } while (0)
#define ARRAY_set(a, i, v) do { ARRAY_check(a, i); (a).begin[i] = v;   \
    } while (0)
#define ARRAY_get(lhs, a, i) do { ARRAY_check(a, i); lhs = (a).begin[i]; \
    } while (0)

#define ARRAY_pop(a) do { ASSERT((a).end!=(a).begin); --(a).end; } while (0)
#define ARRAY_top(a) ((a).end-1)

#define ARRAY_foreach(ty, a, action) do {                               \
        ty* end = (a).end;                                              \
        for (ty* x=(a).begin; x!=end; ++x) { action; }                  \
    } while (0)

#define ARRAY_FIRST_SIZE 4

// nsz >= sz
#define ARRAY_grow_(ty, a, sz, nsz) do {                 \
        ty* nbeg;                                        \
        REALLOC(nbeg, (a).begin, nsz*sizeof(ty));        \
        if (nbeg != (a).begin) {                         \
            (a).begin = nbeg;                            \
            (a).end = nbeg+sz;                           \
        }                                                \
        (a).lim = nbeg+nsz;                              \
    } while (0)

#define ARRAY_resize(ty, a, nsz) do {                           \
        ARRAY_reserve(ty, a, nsz);                              \
        (a).end = (a).begin+nsz;                                \
    } while (0)

#define ARRAY_resizeWith(ty, a, nsz, v) do {                    \
        size_t oldSz = ARRAY_size(a);                           \
        ARRAY_resize(ty, a, nsz);                               \
        for (; oldSz < nsz; ++oldSz) (a).begin[oldSz] = v;      \
    } while (0)

#define ARRAY_reserve(ty, a, capacity) do {                     \
        size_t sz = ARRAY_size(a);                              \
        if (capacity > sz) ARRAY_grow_(ty, a, sz, capacity);    \
    } while (0)

#define ARRAY_contract(ty, a) do {               \
        size_t sz = ARRAY_size(a);               \
        ARRAY_grow_(ty, a, sz, sz);              \
    } while (0)

#define ARRAY_addOne(ty, a) do {                                        \
        if ((a).end == (a).lim) {                                       \
            size_t sz = ARRAY_size(a);                                  \
            ARRAY_grow_(ty, a, sz, (sz ? sz*2 : ARRAY_FIRST_SIZE));     \
        }                                                               \
        ++(a).end;                                                      \
    } while (0)

#define ARRAY_push(ty, a, v) do {                                       \
        ARRAY_addOne(ty, a);                                            \
        *ARRAY_top(a) = v;                                              \
    } while (0)

#endif
