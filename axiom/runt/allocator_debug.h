#ifndef axiom_runt_allocator_debug_H_
#define axiom_runt_allocator_debug_H_

#include "allocator.h"
#include <stdio.h>

void showAllocatorConstants(FILE*);
void showBlock(FILE*, const BlockHeader*);
void showBlocks(FILE*, const BlockHeader*);
void showAllocator(FILE*, const Allocator*);

static inline size_t blockNumUnits_(const BlockHeader* b) {
    return (b->lim-b->base)/(BLOCKUNIT_NUMWORDS);
}

static inline int isTotalSeg_(const BlockHeader* b) {
    uint32_t total = BLOCKFLAG_SEGMENT_HEAD|BLOCKFLAG_SEGMENT_TAIL;
    return (b->flags & total) == total;
}

#endif
