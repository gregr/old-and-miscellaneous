#ifndef axiom_runt_allocator_H_
#define axiom_runt_allocator_H_

#include "types.h"
#include "../common/linked.h"
#include <stddef.h>

#ifndef ALLOCATOR_ALIGNED
#define ALLOCATOR_ALIGNED 1
#endif

// 32-bit words
#define BLOCKHEADER_PADDING 0
// 64-bit words
//#define BLOCKHEADER_PADDING 1

typedef struct BlockHeader_ {
    Word *base, *free, *lim;
    union {
        Word* scan;
    } x;
    struct BlockHeader_* prev;
    struct BlockHeader_* next;
    struct BlockSpace_* owner;
    uint32_t flags;
#if ALLOCATOR_ALIGNED && BLOCKHEADER_PADDING
    uint32_t pad_[BLOCKHEADER_PADDING]; // maintain power of 2 struct size
#endif
} BlockHeader;

static inline int inBlock(BlockHeader* b, Word* p) {
    return (p-b->base) < (b->lim-b->base);
}

#define BLOCKHEADER_SIZE sizeof(BlockHeader)
#define BLOCKUNIT_SIZE_EXP 12
#define BLOCKUNIT_SIZE (1<<BLOCKUNIT_SIZE_EXP)
#define BLOCKUNIT_TOTALSIZE (BLOCKHEADER_SIZE+BLOCKUNIT_SIZE)
#define BLOCKUNIT_NUMWORDS (BLOCKUNIT_SIZE/sizeof(Word))
#define SEGMENT_NUMBLOCKUNITS_EXP 8
#define SEGMENT_DATASIZE (1<<(BLOCKUNIT_SIZE_EXP+SEGMENT_NUMBLOCKUNITS_EXP))

#if ALLOCATOR_ALIGNED
// 32-bit words
#define BLOCKHEADER_SIZE_EXP 5
// 64-bit words
//#define BLOCKHEADER_SIZE_EXP 6
#define ALLOCATOR_NUMBUCKETS SEGMENT_NUMBLOCKUNITS_EXP
#define SEGMENT_NUMBLOCKUNITS (SEGMENT_DATASIZE/BLOCKUNIT_TOTALSIZE)
#define SEGMENT_SIZE SEGMENT_DATASIZE
#define SEGMENT_FIRSTBLOCKOFFSET ((SEGMENT_SIZE-(SEGMENT_NUMBLOCKUNITS*BLOCKUNIT_TOTALSIZE))/BLOCKHEADER_SIZE)
#define ALIGNMENTCHUNK_NUMSEGMENTS 4
#define ALIGNMENTCHUNK_SIZE ((ALIGNMENTCHUNK_NUMSEGMENTS+1)*SEGMENT_SIZE)

// only works for an address within the first block unit
static inline BlockHeader* getBlock(const Word* p) {
    return (BlockHeader*)(((Word)p & ~(SEGMENT_SIZE-1)) |
                          ((((Word)p & (SEGMENT_SIZE-1)) & ~(BLOCKUNIT_SIZE-1))
                           >>(BLOCKUNIT_SIZE_EXP-BLOCKHEADER_SIZE_EXP)));
}

#else
#define ALLOCATOR_NUMBUCKETS (SEGMENT_NUMBLOCKUNITS_EXP+1)
#define SEGMENT_NUMBLOCKUNITS (1<<SEGMENT_NUMBLOCKUNITS_EXP)
#define SEGMENT_SIZE (SEGMENT_NUMBLOCKUNITS*(BLOCKUNIT_SIZE+BLOCKHEADER_SIZE))
#define SEGMENT_FIRSTBLOCKOFFSET 0
#endif

#define BLOCKFLAG_FREE (1<<0)
#define BLOCKFLAG_TAIL (1<<1)
#define BLOCKFLAG_SEGMENT_HEAD (1<<2) // block is first in segment
#define BLOCKFLAG_SEGMENT_TAIL (1<<3) // block is last in segment
#define BLOCKFLAG_COLLECTING (1<<30) // block is being gc'd
#define BLOCKFLAG_SINGLE (1<<31) // mainly for blocks larger than a segment

#if ALLOCATOR_ALIGNED
typedef struct {
    BlockHeader* segs[1];
} AllocAligner;
#endif

typedef struct {
#if ALLOCATOR_ALIGNED
    AllocAligner aligner;
#endif
    BlockHeader* buckets[ALLOCATOR_NUMBUCKETS];
    size_t totalSegs, freeSegs, threshold;
} Allocator;

void Allocator_init(Allocator*);
void Allocator_destroy(Allocator*);
void Allocator_fullCompact(Allocator*);
void Allocator_heuristicCompact(Allocator*);
void Allocator_reserveSegments(Allocator*, size_t);
void Allocator_reserveUnits(Allocator*, size_t);
void Allocator_reserveBytes(Allocator*, size_t);

#define foreachBH_(root, action) LIST_FOREACH(BlockHeader, root, action)

BlockHeader* allocBlockUnits(Allocator*, size_t); // returns null on failure
BlockHeader* allocBlockBytes(Allocator*, size_t); // returns null on failure
void freeBlock(Allocator*, BlockHeader*);
static inline void freeBlocks(Allocator* a, BlockHeader* blocks) {
    foreachBH_(blocks, freeBlock(a, x));
}

static inline size_t Block_nFreeWords(BlockHeader* b) {return b->lim-b->free;}

static inline size_t Block_nFreeBytes(BlockHeader* b) {
    return Block_nFreeWords(b)*sizeof(Word);
}

static inline void setBlocksCollecting(BlockHeader* b) {
    foreachBH_(b, x->flags|=BLOCKFLAG_COLLECTING);
}

static inline void unsetBlocksCollecting(BlockHeader* b) {
    foreachBH_(b, x->flags&=~BLOCKFLAG_COLLECTING);
}

#if ALLOCATOR_ALIGNED
static inline int beingCollected(const Word* p) {
    return getBlock(p)->flags & BLOCKFLAG_COLLECTING;
}

static inline int isAllocated(const Word* p) {
    return p < getBlock(p)->free;
}
#endif

static inline size_t iDivCeil_(size_t dividend, size_t divisor) {
    return ((dividend-1)/divisor)+1;
}

static inline void insertLink_(BlockHeader** root, BlockHeader* b) {
    DLIST_PUSH(*root, b);
}

#define UNLINK_(root, b) DLIST_REMOVE(root, b)

#endif
