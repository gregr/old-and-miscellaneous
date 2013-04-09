#include "node_debug.h"
#include "allocator.h"
#include "../common/array.h"

ARRAY_STRUCT(Layout*, LayoutArray);
static LayoutArray layouts_;

void Layouts_init(void) {
    if (ARRAY_size(layouts_) < TAG_USERDEFINED)
        Layouts_setLargestTag(TAG_USERDEFINED-1);
    Layout* l = initLayout(TAG_EVALUATING);
    l->type = LAYOUT_SMALL_NOPTRS;
    l->x.noPtrs.nWords = 1;
    l = initLayout(TAG_INDIRECT);
    l->type = LAYOUT_INDIRECT;
}

void Layouts_destroy(void) { ARRAY_foreach(Layout*, layouts_, free(x)); }

void Layouts_setLargestTag(Word maxTag) {
    ARRAY_resizeWith(Layout*, layouts_, maxTag+1, 0);
}

Layout* initLayout(Word tag) {
    Layout* l;
    MALLOC(l, sizeof(Layout));
    ARRAY_set(layouts_, tag, l);
    return l;
}

const Layout* getLayout(Word tag) {
    const Layout* l;
    ARRAY_get(l, layouts_, tag);
    return l;
}

size_t nodeSize(const Word* node) {
    const Layout* l = getLayout(*node);
    switch (l->type) {
        case LAYOUT_INDIRECT: return 2;
        case LAYOUT_SMALL_NOPTRS:
        case LAYOUT_LARGE_NOPTRS: return l->x.noPtrs.nWords;
        case LAYOUT_SMALL_PTRSFIRST:
        case LAYOUT_LARGE_PTRSFIRST: return l->x.ptrsFirst.nWords;
        default: return 0; // error
    }
}

////////////////////////////////////////////////////////////////
// debug

void showLayout(FILE* out, const Layout* l) {
    fprintf(out, "type=%d", l->type);
    switch (l->type) {
        case LAYOUT_INDIRECT: fprintf(out, "\n"); break;
        case LAYOUT_SMALL_NOPTRS:
        case LAYOUT_LARGE_NOPTRS:
            fprintf(out, ", nWords=%u\n", l->x.noPtrs.nWords); break;
        case LAYOUT_SMALL_PTRSFIRST:
        case LAYOUT_LARGE_PTRSFIRST:
            fprintf(out, ", nWords=%u, nPtrs=%u\n",
                    l->x.ptrsFirst.nWords, l->x.ptrsFirst.nPtrs); break;
        default: fprintf(out, ", UNKNOWN LAYOUT\n");
    }
}

static inline void showPtr(FILE* out, const Word* p) {
    if (isAllocated(p)) fprintf(out, ", %p", (void*)p);
    else fprintf(out, ", ???%p", (void*)p);
}

void showNode(FILE* out, const Word* node) {
    if (!isAllocated(node)) fprintf(out, "??? ");
    fprintf(out, "{%d", *node);
    const Layout* l = getLayout(*node);
    switch (l->type) {
        case LAYOUT_INDIRECT: showPtr(out, (Word*)node[INDIRECT_PTR]); break;
        case LAYOUT_SMALL_NOPTRS:
        case LAYOUT_LARGE_NOPTRS: {
            for (size_t itr = 1; itr != l->x.noPtrs.nWords; ++itr)
                fprintf(out, ", %x", node[itr]);
        } break;
        case LAYOUT_SMALL_PTRSFIRST:
        case LAYOUT_LARGE_PTRSFIRST: {
            size_t itr = 1;
            for (; itr < l->x.ptrsFirst.nPtrs+1; ++itr)
                showPtr(out, (Word*)node[itr]);
            for (; itr != l->x.ptrsFirst.nWords; ++itr)
                fprintf(out, ", %x", node[itr]);
        } break;
        default: fprintf(out, ", UNKNOWN NODE\n");
    }
    fprintf(out, "}\n");
}

void showAddrNode(FILE* out, const Word* node) {
    fprintf(out, "%p: ", (void*)node);
    showNode(out, node);
}
