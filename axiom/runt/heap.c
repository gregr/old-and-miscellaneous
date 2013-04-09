#include "heap_debug.h"
#include "node_debug.h"
#include "allocator_debug.h"
#include <string.h>

/* static inline void push_(BlockHeader** tgt, BlockHeader* b) { */
/*     b->next = *tgt; */
/*     *tgt = b; */
/* } */

/* static inline BlockHeader* pop_(BlockHeader** src) { */
/*     BlockHeader* b = *src; */
/*     *src = b->next; */
/*     return b; */
/* } */

static BlockHeader* allocNewRootBlock_(RootStack* rs) {
    if (rs->extra == 0) {
        BlockHeader* b = allocBlockUnits(rs->a, 1);
        if (b != 0) SLIST_PUSH(rs->top, b);
        return b;
    } else {
        SLIST_PUSH(rs->top, rs->extra);
        rs->extra = 0;
        return rs->top;
    }
}

int RootStack_init(RootStack* rs, Allocator* a) {
    rs->a = a;
    rs->extra = 0;
    rs->top = 0;
    return allocNewRootBlock_(rs) != 0;
}

void RootStack_destroy(RootStack* rs) {
    freeBlocks(rs->a, rs->top);
    freeBlocks(rs->a, rs->extra);
}

Word** RootStack_push(RootStack* rs, size_t nRoots) {
    // todo: assumption that nRoots <= BLOCKUNIT_NUMWORDS ...
    BlockHeader* b = rs->top;
    if ((size_t)(b->lim - b->free) < nRoots) {
        b = allocNewRootBlock_(rs);
        if (b == 0) return 0;
    }
    Word** roots = (Word**)b->free;
    b->free += nRoots;
    return roots;
}

void RootStack_pop(RootStack* rs, size_t nRoots) {
    // assumes never popping more than the number of roots in top block at once
    BlockHeader* b = rs->top;
    b->free -= nRoots;
    if ((b->free == b->base) && (b->next != 0)) {
        freeBlocks(rs->a, rs->extra);
        SLIST_POP(rs->extra, rs->top);
    }
}

static BlockHeader* allocNewBlock_(BlockSpace* bs) {
    BlockHeader* b = allocBlockUnits(bs->heap->a, 1);
    if (b != 0) {
        b->owner = bs;
        SLIST_PUSH(bs->to, b);
        ++bs->numBlocks;
    }
    return b;
}

static BlockHeader* allocNewLargeBlock_(BlockSpace* bs, size_t nWords) {
    BlockHeader* b = allocBlockBytes(bs->heap->a, nWords*sizeof(Word));
    if (b != 0) {
        b->owner = bs;
        insertLink_(&bs->largeTo, b);
        ++bs->numBlocks;
    }
    return b;
}

static int BlockSpace_init(BlockSpace* bs, Heap* h) {
    bs->heap = h;
    bs->to = 0;
    bs->largeTo = 0;
    bs->from = 0;
    bs->largeFrom = 0;
    bs->scan = 0;
    bs->pending = 0;
    bs->finished = 0;
    bs->numBlocks = 0;
    bs->limBlocks = 2;
    return allocNewBlock_(bs) != 0;
}

static void BlockSpace_chooseLimit(BlockSpace* bs) {
    if (bs->numBlocks > bs->limBlocks/2) bs->limBlocks *= 2;
    else if (bs->numBlocks < bs->limBlocks/4) bs->limBlocks /= 2;
}

static void BlockSpace_destroy(BlockSpace* bs) {
    freeBlocks(bs->heap->a, bs->to);
    freeBlocks(bs->heap->a, bs->largeTo);
}

int Heap_init(Heap* h, RootStack* rs, Allocator* a) {
    h->a = a;
    h->rs = rs;
    return BlockSpace_init(&h->bs, h);
}

void Heap_destroy(Heap* h) {
    BlockSpace_destroy(&h->bs);
}

#define FREE_SPACE_(bs, nWords, allocCall) do {                \
        if (bs->limBlocks > bs->numBlocks) {                   \
            if ((allocCall) == 0) gcSpace(bs, nWords);         \
        } else gcSpace(bs, nWords);                            \
    } while (0)

Word* allocSpaceWords(BlockSpace* bs, size_t nWords) {
    if (nWords < BLOCKUNIT_NUMWORDS) {
        size_t freeWords = Block_nFreeWords(bs->to);
        if (freeWords < nWords) FREE_SPACE_(bs, nWords, allocNewBlock_(bs));
        Word* p = bs->to->free;
        bs->to->free += nWords;
        return p;
    } else { // todo: limLargeBlocks ?
        FREE_SPACE_(bs, nWords, allocNewLargeBlock_(bs, nWords));
        return bs->largeTo->free;
    }
}

Word* allocSpaceBytes(BlockSpace* bs, size_t nBytes) {
    return allocSpaceWords(bs, iDivCeil_(nBytes, sizeof(Word)));
}

Word* allocWords(Heap* h, size_t nWords) {
    return allocSpaceWords(&h->bs, nWords);
}

Word* allocBytes(Heap* h, size_t nBytes) {
    return allocSpaceBytes(&h->bs, nBytes);
}

static void gcFail_(void) {} // todo

static inline Word* evacuateWords(BlockSpace* dst, Word* p, size_t nWords) {
    BlockHeader* to = dst->to;
    size_t freeWords = Block_nFreeWords(to);
    if (freeWords < nWords) {
        if (to != dst->scan) SLIST_PUSH(dst->pending, to);
        BlockHeader* b = allocNewBlock_(dst);
        if (b != 0) {
            b->x.scan = b->free;
            dst->to = b;
        } else gcFail_();
    }
    Word* pnew = to->free;
    to->free += nWords;
    memcpy(pnew, p, nWords*sizeof(Word));
    setIndirect(p, pnew);
    return pnew;
}

static inline void evacuateLarge(BlockSpace* dst, BlockSpace* src,
                                 BlockHeader* b) {
    UNLINK_(src->largeFrom, b);
    b->flags &= ~BLOCKFLAG_COLLECTING;
    b->owner = dst;
    SLIST_PUSH(dst->pending, b);
}

void evacuate(BlockSpace*, Word**);

static inline Word* evacuateIndirect(BlockSpace* dst, Word* ind) {
    Word** pp = (Word**)(ind+INDIRECT_PTR);
    evacuate(dst, pp);
    return *pp;
}

static inline int isLargeBlock_(BlockHeader* b) {
    return (b->lim-b->free) > (signed)BLOCKUNIT_NUMWORDS;
}

void evacuate(BlockSpace* dst, Word** pp) {
    Word* p = *pp;
    if (beingCollected(p)) {
        const Layout* layout = getLayout(*p);
        switch (layout->type) {
            case LAYOUT_INDIRECT: *pp = evacuateIndirect(dst, p); break;
            case LAYOUT_SMALL_NOPTRS:
                *pp = evacuateWords(dst, p, layout->x.noPtrs.nWords);
                break;
            case LAYOUT_SMALL_PTRSFIRST:
                *pp = evacuateWords(dst, p, layout->x.ptrsFirst.nWords);
                break;
            case LAYOUT_LARGE_NOPTRS: case LAYOUT_LARGE_PTRSFIRST: {
                BlockHeader* b = getBlock(p);
                evacuateLarge(dst, b->owner, b);
            } break;
            default: break; // todo: assert never
        }
    }
}

static inline Word* scavenge(BlockSpace* dst, Word* scan) {
    const Layout* layout = getLayout(*scan);
    switch (layout->type) {
        case LAYOUT_SMALL_NOPTRS: case LAYOUT_LARGE_NOPTRS:
            return scan+layout->x.noPtrs.nWords;
        case LAYOUT_SMALL_PTRSFIRST: case LAYOUT_LARGE_PTRSFIRST: {
            Word** ptr = (Word**)(scan+1);
            Word** ptrEnd = ptr + layout->x.ptrsFirst.nPtrs; // todo
            for (; ptr != ptrEnd; ++ptr) evacuate(dst, ptr);
            return scan+layout->x.ptrsFirst.nWords;
        }
        default: return 0; // todo: assert that this never happens
    };
}

void scavengeBlock(BlockSpace* dst, BlockHeader* b) {
    Word* scan = b->x.scan;
    while (scan != b->free) scan = scavenge(dst, scan);
    b->x.scan = scan;
}

void scan(BlockSpace* bs) {
    bs->scan = 0;
    for (;;) {
        if (bs->pending != 0) SLIST_POP(bs->scan, bs->pending);
        else {
            if (bs->scan != bs->to) bs->scan = bs->to;
            else return;
        }
        scavengeBlock(bs, bs->scan);
        if (bs->scan != bs->to) {
            if (!isLargeBlock_(bs->scan)) SLIST_PUSH(bs->finished, bs->scan);
            else insertLink_(&bs->largeTo, bs->scan);
        }
    }
}

#define FOREACH_ROOT(rs, action) do {           \
        BlockHeader* b = rs->top;               \
        while (b != 0) {                                                \
            Word **root = (Word**)b->base, **rootLim = (Word**)b->free; \
            for (; root != rootLim; ++root) action;                     \
            b = b->next;                                                \
        }                                                               \
} while (0)

static inline void evacuateRoots(BlockSpace* bs, RootStack* rs) {
    FOREACH_ROOT(rs, evacuate(bs, root));
}

void gcSpace(BlockSpace* bs, size_t requiredWords) {
    bs->from = bs->to;
    bs->largeFrom = bs->largeTo;
    bs->to = 0;
    bs->largeTo = 0;
    bs->numBlocks = 0;
    BlockHeader* b;
    if ((b = allocNewBlock_(bs)) != 0) {
        b->x.scan = b->free;
        setBlocksCollecting(bs->from);
        setBlocksCollecting(bs->largeFrom);
        evacuateRoots(bs, bs->heap->rs);
        printf("finished roots\n");
        scan(bs);
        printf("finished scan\n");
        // garbage collection is now finished
        SLIST_PUSH(bs->finished, bs->to);
        bs->to = bs->finished;
        unsetBlocksCollecting(bs->from);
        unsetBlocksCollecting(bs->largeFrom);
        freeBlocks(bs->heap->a, bs->from);
        freeBlocks(bs->heap->a, bs->largeFrom);
        bs->from = 0;
        bs->largeFrom = 0;
        bs->scan = 0;
        bs->pending = 0;
        bs->finished = 0;
        if (requiredWords > Block_nFreeWords(bs->to)) // one should be enough
            if (allocNewBlock_(bs) == 0) gcFail_();
        BlockSpace_chooseLimit(bs);
    } else gcFail_();
}

void gcHeap(Heap* h, size_t requiredWords) { gcSpace(&h->bs, requiredWords); }

////////////////////////////////////////////////////////////////
// debug

void showRoots(FILE* out, const RootStack* rs) {
    size_t nRoots = 0;
    FOREACH_ROOT(rs, ++nRoots);
    fprintf(out, "nRoots=%d\n", nRoots);
    FOREACH_ROOT(rs, {
            --nRoots;
            fprintf(out, "-%d: ", nRoots);
            showAddrNode(out, *root);
        });
}

void showBlockNodes(FILE* out, const BlockHeader* b) {
    showBlock(out, b);
    const Word* node = b->base;
    while (node < b->free) {
        showAddrNode(out, node);
        node += nodeSize(node);
    }
}

void showBlocksNodes(FILE* out, const BlockHeader* b) {
    if (b != 0) {
        for (;;) {
            showBlockNodes(out, b);
            b = b->next;
            if (b == 0) break;
            fprintf(out, "================\n");
        }
    } else fprintf(out, "null\n");
}

#define SHOW_BLOCK_NODES_(block) do {                 \
        fprintf(out, "================\n"#block": "); \
        showBlocksNodes(out, bs->block);           \
    } while (0)

void showBlockSpace(FILE* out, const BlockSpace* bs) {
    fprintf(out, "numBlocks=%u, limBlocks=%u\n", bs->numBlocks, bs->limBlocks);
    SHOW_BLOCK_NODES_(to);
    SHOW_BLOCK_NODES_(largeTo);
    SHOW_BLOCK_NODES_(from);
    SHOW_BLOCK_NODES_(largeFrom);
    SHOW_BLOCK_NODES_(scan);
    SHOW_BLOCK_NODES_(pending);
    SHOW_BLOCK_NODES_(finished);
}

void showHeap(FILE* out, const Heap* h) {
    showBlockSpace(out, &h->bs);
    fprintf(out, "================\n");
    showRoots(out, h->rs);
}
