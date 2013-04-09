#ifndef axiom_runt_heap_H_
#define axiom_runt_heap_H_

#include "allocator.h"

typedef struct BlockSpace_ {
    struct Heap_* heap;
    BlockHeader *to, *largeTo, *from, *largeFrom;
    BlockHeader *scan, *pending, *finished; // large finished go to largeTo
    size_t numBlocks, limBlocks;
} BlockSpace;

typedef struct {
    Allocator* a;
    BlockHeader *top, *extra;
} RootStack;

typedef struct Heap_ {
    BlockSpace bs;
    Allocator* a;
    RootStack* rs;
} Heap;

int Heap_init(Heap*, RootStack*, Allocator*);
void Heap_destroy(Heap*);

Word* allocSpaceWords(BlockSpace*, size_t);
Word* allocSpaceBytes(BlockSpace*, size_t);

Word* allocWords(Heap*, size_t);
Word* allocBytes(Heap*, size_t);

////////////////////////////////////////////////////////////////
// roots
int RootStack_init(RootStack*, Allocator*);
void RootStack_destroy(RootStack*);

Word** RootStack_push(RootStack*, size_t);
void RootStack_pop(RootStack*, size_t);

////////////////////////////////////////////////////////////////
// garbage collection
void gcSpace(BlockSpace*, size_t);
void gcHeap(Heap*, size_t);

static inline int shouldGC(Heap* h) {
    BlockSpace* bs = &h->bs;
    return bs->limBlocks==bs->numBlocks &&
        (Block_nFreeWords(bs->to) < (BLOCKUNIT_NUMWORDS>>1));
}

static inline void incBlockLimit(BlockSpace* bs, size_t nBlocks) {
    bs->limBlocks += nBlocks;
}

#endif
