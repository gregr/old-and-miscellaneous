#ifndef axiom_runt_node_H_
#define axiom_runt_node_H_

#include "types.h"
#include <stddef.h>

enum { TAG_EVALUATING, TAG_INDIRECT, TAG_USERDEFINED };

typedef enum { LAYOUT_INDIRECT,
               LAYOUT_SMALL_NOPTRS, LAYOUT_LARGE_NOPTRS,
               LAYOUT_SMALL_PTRSFIRST, LAYOUT_LARGE_PTRSFIRST,
               //    LAYOUT_SMALL_ARRAY, LAYOUT_LARGE_ARRAY,
               //    LAYOUT_SMALL_ARRAY_NOPTRS, LAYOUT_LARGE_ARRAY_NOPTRS,
               LAYOUT_NUMLAYOUTS
} LayoutType;

#define INDIRECT_PTR 1

static inline Word* indirectPtr(Word* p) { return (Word*)p[INDIRECT_PTR]; }

static inline void setIndirect(Word* p, Word* pnew) {
    p[INDIRECT_PTR] = (Word)pnew;
    *p = TAG_INDIRECT;
}

typedef struct {
    LayoutType type;
    union {
        struct { size_t nWords; } noPtrs;
        struct { size_t nWords, nPtrs; } ptrsFirst; // could subsume noptrs?
        struct { size_t nWords; } scattered; // bitmap idea
    } x;
} Layout;

void Layouts_init(void);
void Layouts_destroy(void);
void Layouts_setLargestTag(Word);
Layout* initLayout(Word);
const Layout* getLayout(Word);

size_t nodeSize(const Word*);

#endif
