#include "allocator_debug.h"
#include <stdlib.h>

#define RELINK_(root, src, tgt) DLIST_POPPUSH(root, src, tgt)

static inline void inheritSegTail_(BlockHeader* src, BlockHeader* tgt) {
    if (src->flags & BLOCKFLAG_SEGMENT_TAIL) // todo: eliminate branch
        tgt->flags |= BLOCKFLAG_SEGMENT_TAIL;
    src->flags &= ~BLOCKFLAG_SEGMENT_TAIL;
}

static inline void repointTail_(BlockHeader* head, BlockHeader* tail) {
    if (tail != head) {
        tail->flags |= BLOCKFLAG_TAIL;
        tail->next = head;
    } else head->flags &= ~BLOCKFLAG_TAIL;
}

static inline void pointTail_(BlockHeader* head, BlockHeader* tail) {
    if (tail != head) {
        tail->flags = BLOCKFLAG_TAIL;
        tail->next = head;
    }
}

static inline void mergeAdj_(BlockHeader* lhs, BlockHeader* rhs) {
    lhs->lim = rhs->lim;
    inheritSegTail_(rhs, lhs);
}

#define TRYMERGE_(root, b, offRight, offLeft, unlinkLHS) do {           \
        BlockHeader* adj;                                               \
        if (!(b->flags & BLOCKFLAG_SEGMENT_TAIL)) {                     \
            adj = offRight;                                             \
            if (adj->flags & BLOCKFLAG_FREE) {                          \
                UNLINK_(root, adj);                                     \
                mergeAdj_(b, adj);                                      \
            }                                                           \
        }                                                               \
        if (!(b->flags & BLOCKFLAG_SEGMENT_HEAD)) {                     \
            adj = offLeft;                                              \
            if (adj->flags & BLOCKFLAG_TAIL) /* elim branch */          \
                adj = adj->next;                                        \
            if (adj->flags & BLOCKFLAG_FREE) {                          \
                if (unlinkLHS) UNLINK_(root, adj);                      \
                mergeAdj_(adj, b);                                      \
                b = adj;                                                \
            }                                                           \
        }                                                               \
        b->flags |= BLOCKFLAG_FREE;                                     \
    } while (0)

#if ALLOCATOR_ALIGNED
static void AllocAligner_init(AllocAligner* aa) {
    *aa->segs = 0;
}

static void AllocAligner_destroy(AllocAligner* aa) {
    foreachBH_(*aa->segs, free(x->base));
}

static inline BlockHeader* offsetSeg_(BlockHeader* seg, size_t offset) {
    return (BlockHeader*)((uint8_t*)seg+(offset*SEGMENT_SIZE));
}

static inline void initChunk_(uint8_t* chunk, BlockHeader* seg) {
    seg->flags = BLOCKFLAG_FREE|BLOCKFLAG_SEGMENT_HEAD|BLOCKFLAG_SEGMENT_TAIL;
    seg->base = (Word*)chunk;
    seg->lim = (Word*)offsetSeg_(seg, ALIGNMENTCHUNK_NUMSEGMENTS);
    if (ALIGNMENTCHUNK_NUMSEGMENTS > 1) {
        BlockHeader* tail = offsetSeg_(seg, ALIGNMENTCHUNK_NUMSEGMENTS-1);
        tail->flags = BLOCKFLAG_TAIL;
        tail->next = seg;
    }
}

static BlockHeader* AllocAligner_allocChunk(AllocAligner* aa) {
    uint8_t* chunk = malloc(ALIGNMENTCHUNK_SIZE);
    if (chunk != 0) {
        BlockHeader* seg = (BlockHeader*)(((Word)chunk&~(SEGMENT_SIZE-1))
                                          +SEGMENT_SIZE);
        //        printf("mask=%x, chunk=%x, seg=%x, diff=%u\n", ~(SEGMENT_SIZE-1), (size_t)chunk, (size_t)seg, (uint8_t*)seg-chunk);
        initChunk_(chunk, seg);
        insertLink_(aa->segs, seg);
        return seg;
    } else return 0;
}

static BlockHeader* splitTopFreeSeg_(AllocAligner* aa) {
    BlockHeader *next, *seg = *aa->segs;
    next = offsetSeg_(seg, 1);
    if (seg->lim > (Word*)next) {
        next->lim = seg->lim;
        next->flags = BLOCKFLAG_FREE;
        inheritSegTail_(seg, next);
        RELINK_(*aa->segs, seg, next);
        pointTail_(next, offsetSeg_((BlockHeader*)seg->lim, -1));
        seg->lim = (Word*)next;
    } else {
        UNLINK_(*aa->segs, seg);
    }
    seg->flags &= ~BLOCKFLAG_FREE;
    seg->prev = seg->next = 0;
    return seg+SEGMENT_FIRSTBLOCKOFFSET;
}

static inline BlockHeader* allocAlignedSeg_(AllocAligner* aa) {
    if (*aa->segs == 0) if (AllocAligner_allocChunk(aa) == 0) return 0;
    return splitTopFreeSeg_(aa);
}

static void freeAlignedSegment_(Allocator* a, BlockHeader* seg) {
    TRYMERGE_(*(a->aligner.segs), seg, offsetSeg_(seg, 1), offsetSeg_(seg, -1),
              0);
    pointTail_(seg, offsetSeg_((BlockHeader*)seg->lim, -1));
    if (isTotalSeg_(seg)) free(seg->base);
}
#endif

static inline void freeSegment(Allocator* a, BlockHeader* b) {
#if ALLOCATOR_ALIGNED
    freeAlignedSegment_(a, b-SEGMENT_FIRSTBLOCKOFFSET);
#else
    free(b);
#endif
}

static inline void freeSingle(BlockHeader* b) { free(b); }

void Allocator_init(Allocator* a) {
#if ALLOCATOR_ALIGNED
    AllocAligner_init(&a->aligner);
#endif
    BlockHeader **b, **end;
    b = a->buckets;
    end = b+ALLOCATOR_NUMBUCKETS;
    for (; b != end; ++b) *b = 0;
    a->freeSegs = a->totalSegs = 0;
    a->threshold = 1;
}

void Allocator_destroy(Allocator* a) {
#if ALLOCATOR_ALIGNED
    AllocAligner_destroy(&a->aligner);
#endif
}

void Allocator_fullCompact(Allocator* a) {
    BlockHeader** bucket = a->buckets+ALLOCATOR_NUMBUCKETS-1;
    foreachBH_(*bucket, if (isTotalSeg_(x)) freeSegment(a, x));
    *bucket = 0;
    a->totalSegs -= a->freeSegs;
    a->freeSegs = 0;
}

void Allocator_heuristicCompact(Allocator* a) {
    if ((a->freeSegs > a->threshold) &&
        (a->freeSegs >= iDivCeil_(3*a->totalSegs, 4))) {
        BlockHeader** bucket = a->buckets+ALLOCATOR_NUMBUCKETS-1;
        BlockHeader *block = *bucket, *next;
        size_t toRemove = a->totalSegs/2;
        a->freeSegs -= toRemove;
        a->totalSegs -= toRemove;
        while (toRemove > 0) {
            next = block->next;
            if (isTotalSeg_(block)) {
                freeSegment(a, block);
                --toRemove;
            }
            block = next;
        }
        *bucket = block;
    }
}

static inline void initSegment_(BlockHeader* seg) {
    Word* base = (Word*)(seg+SEGMENT_NUMBLOCKUNITS);
    seg->free = seg->base = base;
    seg->lim = base+BLOCKUNIT_NUMWORDS*SEGMENT_NUMBLOCKUNITS;
    seg->flags = BLOCKFLAG_FREE|
        BLOCKFLAG_SEGMENT_HEAD|BLOCKFLAG_SEGMENT_TAIL;
    if (SEGMENT_NUMBLOCKUNITS > 1) {
        BlockHeader* tail = (seg+SEGMENT_NUMBLOCKUNITS-1);
        tail->flags = BLOCKFLAG_TAIL;
        tail->next = seg;
    }
}

static BlockHeader* allocSegment(Allocator* a) {
#if ALLOCATOR_ALIGNED
    BlockHeader* seg = allocAlignedSeg_(&a->aligner);
#else
    BlockHeader* seg = malloc(SEGMENT_SIZE);
#endif
    if (seg != 0) {
        initSegment_(seg);
        insertLink_(a->buckets+(ALLOCATOR_NUMBUCKETS-1), seg);
        ++a->totalSegs;
        ++a->freeSegs;
    }
    return seg;
}

static BlockHeader* allocSingle(size_t sz) {
    BlockHeader* b = malloc(sz+BLOCKHEADER_SIZE);
    if (b != 0) {
        Word* base = (Word*)(b+1);
        b->free = b->base = base;
        b->lim = base+(sz/sizeof(Word));
        b->flags = BLOCKFLAG_SINGLE;
    }
    return b;
}

void Allocator_reserveSegments(Allocator* a, size_t numSegs) {
    a->threshold = numSegs*3;
    if (numSegs > a->totalSegs) {
        numSegs -= a->totalSegs;
        for (; numSegs > 0; --numSegs)
            if (allocSegment(a) == 0) break;
    }
}

void Allocator_reserveUnits(Allocator* a, size_t units) {
    Allocator_reserveSegments(a, iDivCeil_(units, SEGMENT_NUMBLOCKUNITS));
}

void Allocator_reserveBytes(Allocator* a, size_t sz) {
    Allocator_reserveSegments(a, iDivCeil_(sz, SEGMENT_DATASIZE));
}

static inline size_t iLog2Floor_(size_t n) {
    size_t v = 1, r = 0;
    for (; v <= n; ++r) v = v << 1;
    return r-1;
}

static inline size_t iLog2Ceil_(size_t n) {
    size_t v = 1, r = 0;
    for (; v < n; ++r) v = v << 1;
    return r;
}

static void splitTopFreeBlock_(Allocator* a, BlockHeader** bucket,
                               BlockHeader* b, size_t requestedUnits) {
    size_t totalUnits = blockNumUnits_(b);
    size_t diff = totalUnits - requestedUnits;
    BlockHeader* bnew;
    if (diff != 0) {
        bnew = b+requestedUnits;
        pointTail_(b, bnew-1);
        repointTail_(bnew, b+totalUnits-1);
        bnew->flags = BLOCKFLAG_FREE;
        inheritSegTail_(b, bnew);
        bnew->lim = b->lim;
        b->lim -= (diff*BLOCKUNIT_NUMWORDS);
        bnew->free = bnew->base = b->lim;
        bnew->next = b->next;
        bnew->prev = 0;
    }
    if (b->next != 0) b->next->prev = 0;
    *bucket = b->next;
    b->flags &= ~BLOCKFLAG_FREE;
    b->prev = b->next = 0;
    if (totalUnits == SEGMENT_NUMBLOCKUNITS) --a->freeSegs; // branch
    if (diff != 0) {
        insertLink_(a->buckets+iLog2Floor_(diff), bnew);
    }
}

BlockHeader* allocBlockUnits_(Allocator* a, size_t numUnits) {
    if (numUnits == 0) return 0;
    size_t bucketIndex = iLog2Ceil_(numUnits);
    BlockHeader **bucket = a->buckets+bucketIndex,
        **bucketEnd = a->buckets+ALLOCATOR_NUMBUCKETS;
    BlockHeader* b;
    for (; bucket != bucketEnd; ++bucket) {
        if ((b = *bucket) != 0) {
            splitTopFreeBlock_(a, bucket, b, numUnits);
            return b;
        }
    }
    b = allocSegment(a); // failed to find an existing free block
    if (b != 0) splitTopFreeBlock_(a, bucketEnd-1, b, numUnits);
    return b;
}

BlockHeader* allocBlockBytes(Allocator* a, size_t sz) {
    size_t numUnits = iDivCeil_(sz, BLOCKUNIT_SIZE);
    if (numUnits <= SEGMENT_NUMBLOCKUNITS) {
        return allocBlockUnits_(a, numUnits);
    } else return allocSingle(sz);
}

BlockHeader* allocBlockUnits(Allocator* a, size_t numUnits) {
    if (numUnits <= SEGMENT_NUMBLOCKUNITS) {
        return allocBlockUnits_(a, numUnits);
    } else return allocSingle(numUnits*BLOCKUNIT_SIZE);
}

void freeBlock(Allocator* a, BlockHeader* b) {
    if (!(b->flags & BLOCKFLAG_SINGLE)) {
        TRYMERGE_(a->buckets[iLog2Floor_(blockNumUnits_(adj))], b,
                  b+blockNumUnits_(b), b-1, 1);
        b->free = b->base;
        size_t numUnits = blockNumUnits_(b);
        pointTail_(b, b+numUnits-1);
        insertLink_(a->buckets+iLog2Floor_(numUnits), b);
        if (isTotalSeg_(b)) {
            ++a->freeSegs;
            Allocator_heuristicCompact(a);
        }
    } else freeSingle(b);
}

////////////////////////////////////////////////////////////////
// debug

void showAllocatorConstants(FILE* out) {
    fprintf(out, "BLOCKHEADER_SIZE = %u\nBLOCKUNIT_SIZE_EXP = %u\n"
            "BLOCKUNIT_SIZE = %u\nBLOCKUNIT_TOTALSIZE = %u\n"
            "BLOCKUNIT_NUMWORDS = %u\n"
            "SEGMENT_NUMBLOCKUNITS_EXP = %u\nSEGMENT_NUMBLOCKUNITS = %u\n"
            "SEGMENT_DATASIZE = %u\nSEGMENT_SIZE = %u\n"
            "SEGMENT_FIRSTBLOCKOFFSET = %u\n"
            "ALLOCATOR_NUMBUCKETS = %u\n"
#if ALLOCATOR_ALIGNED
            "ALIGNMENTCHUNK_NUMSEGMENTS = %u\nALIGNMENTCHUNK_SIZE = %u\n"
#endif
            "BLOCKFLAG_FREE = %u\nBLOCKFLAG_TAIL = %u\n"
            "BLOCKFLAG_SEGMENT_HEAD = %u\nBLOCKFLAG_SEGMENT_TAIL = %u\n"
            "BLOCKFLAG_SINGLE = %u\n\n",
            BLOCKHEADER_SIZE, BLOCKUNIT_SIZE_EXP,
            BLOCKUNIT_SIZE, BLOCKUNIT_TOTALSIZE, BLOCKUNIT_NUMWORDS,
            SEGMENT_NUMBLOCKUNITS_EXP, SEGMENT_NUMBLOCKUNITS,
            SEGMENT_DATASIZE, SEGMENT_SIZE, SEGMENT_FIRSTBLOCKOFFSET,
            ALLOCATOR_NUMBUCKETS,
#if ALLOCATOR_ALIGNED
            ALIGNMENTCHUNK_NUMSEGMENTS, ALIGNMENTCHUNK_SIZE,
#endif
            BLOCKFLAG_FREE, BLOCKFLAG_TAIL,
            BLOCKFLAG_SEGMENT_HEAD, BLOCKFLAG_SEGMENT_TAIL,
            BLOCKFLAG_SINGLE);
}

void showBlock(FILE* out, const BlockHeader* b) {
    fprintf(out, "block=%p, base=%p, free=%p, lim=%p, "
           "totalWords=%u, freeWords=%u, flags=%016x\n",
           (void*)b, (void*)b->base, (void*)b->free, (void*)b->lim,
           b->lim-b->base, b->lim-b->free, b->flags);
}

void showBlocks(FILE* out, const BlockHeader* b) {
    if (b != 0) {
        while (b != 0) {
            showBlock(out, b);
            b = b->next;
        }
    } else fprintf(out, "null\n");
}

void showAllocator(FILE* out, const Allocator* a) {
    fprintf(out, "totalSegs=%u, freeSegs=%u, threshold=%u\n================\n",
           a->totalSegs, a->freeSegs, a->threshold);
    for (size_t i = 0; i < ALLOCATOR_NUMBUCKETS; ++i) {
        fprintf(out, "bucket=%u, minUnitsPerBlock=%u, minWordsPerBlock=%u\n"
               "================\n",
               i, 1<<i, (1<<i)*BLOCKUNIT_SIZE/sizeof(Word));
        showBlocks(out, a->buckets[i]);
        fprintf(out, "================\n");
    }
}
