#include "allocator_debug.h"
//#define TEST_VERBOSE 1
#include "../common/testing.h"

// todo: check aligner chunks?

int validateLeftNeighborNotFree(const BlockHeader* b) {
    int result_ = 1;
    if (!(b->flags & BLOCKFLAG_SEGMENT_HEAD)) {
        --b;
        if (b->flags & BLOCKFLAG_TAIL) b = b->next;
        TEST_(!(b->flags & BLOCKFLAG_FREE));
    }
    return result_;
}

int validateConstants_(void) {
    int result_ = 1;
#if ALLOCATOR_ALIGNED
    TEST_CMPU_(BLOCKHEADER_SIZE, ==, 32);
    //    TEST_CMPU_(BLOCKHEADER_SIZE, ==, 64);
    TEST_CMPU_(SEGMENT_FIRSTBLOCKOFFSET, >, 0);
    TEST_CMPU_(1<<BLOCKHEADER_SIZE_EXP, ==, BLOCKHEADER_SIZE);
#endif
    TEST_CMPU_(BLOCKUNIT_NUMWORDS, >, 1);
    return result_;
}

int validateBlock_(const BlockHeader* b) {
    int result_ = 1;
    TEST_CMPP_(b->lim, >, b->base);
    TEST_CMPP_(b->free, >=, b->base);
    TEST_CMPP_(b->free, <=, b->lim);
    size_t numUnits = blockNumUnits_(b);
    if (isTotalSeg_(b)) TEST_CMPU_(numUnits, ==, SEGMENT_NUMBLOCKUNITS);
    if (numUnits > 1) {
        if (!(b->flags & BLOCKFLAG_SEGMENT_TAIL)) {
            const BlockHeader* tail = (b+numUnits-1);
            TEST_CMPX_(tail->flags, &, BLOCKFLAG_TAIL);
            TEST_CMPP_(tail->next, ==, b);
        }
    }
    if (b->flags & BLOCKFLAG_FREE) TEST_(validateLeftNeighborNotFree(b));
#if ALLOCATOR_ALIGNED
    TEST_CMPP_(b, ==, getBlock(b->base+1));
    TEST_CMPP_(b, ==, getBlock(b->base+BLOCKUNIT_NUMWORDS-2));
#endif
    return result_;
}

int validateBlocks_(const BlockHeader* b) {
    int result_ = 1;
    foreachBH_((BlockHeader*)b, TEST_(validateBlock_(x)));
    return result_;
}

int validateAllocator_(const Allocator* a) {
    int result_ = 1;
    size_t freeSegs = 0;
    const BlockHeader* b = a->buckets[ALLOCATOR_NUMBUCKETS-1];
    while (b != 0) {
        if (isTotalSeg_(b)) ++freeSegs;
        b = b->next;
    }
    TEST_CMPU_(a->freeSegs, ==, freeSegs);
    TEST_CMPU_(a->totalSegs, >=, a->freeSegs);
    for (size_t i = 0; i < ALLOCATOR_NUMBUCKETS; ++i)
        TEST_(validateBlocks_(a->buckets[i]));
    return result_;
}

int test_(void) {
    int result_;
    TEST_(validateConstants_());
    Allocator a;
    Allocator_init(&a);
    TEST_(validateAllocator_(&a));
    Allocator_reserveSegments(&a, 8);
    TEST_(validateAllocator_(&a));
    BlockHeader* b = allocBlockUnits(&a, 3);
    TEST_(validateAllocator_(&a));
    TEST_(validateBlock_(b));
    BlockHeader* b2 = allocBlockUnits(&a, 1);
    TEST_(validateAllocator_(&a));
    TEST_(validateBlock_(b2));
    BlockHeader* b3 = allocBlockUnits(&a, 4);
    TEST_(validateAllocator_(&a));
    TEST_(validateBlock_(b3));
    freeBlock(&a, b);
    TEST_(validateAllocator_(&a));
    freeBlock(&a, b3);
    TEST_(validateAllocator_(&a));
    freeBlock(&a, b2);
    TEST_(validateAllocator_(&a));
    Allocator_destroy(&a);
    return result_;
}

int main() {
    if (test_()) printf("all tests passed\n");
    else printf("tests failed\n");
/*     FILE* out = stdout; */
/*     showAllocatorConstants(out); */
/*     Allocator a; */
/*     Allocator_init(&a); */
/*     //    Allocator_reserve(&a, 1); */

/*     Allocator_reserveSegments(&a, 8); */
/*     //    printf("freeSegs=%u\n", a.freeSegs); */
/*     //    a.threshold = 1; */
/*     //    showAllocator(out, &a); */
/*     //    showBlocks(out, a.buckets[ALLOCATOR_NUMBUCKETS - 1]); */

/*     BlockHeader* b = allocBlockUnits(&a, 3); */
/*     //    printf("freeSegs=%u\n", a.freeSegs); */
/*     BlockHeader* b2 = allocBlockUnits(&a, 4); */
/*     //    printf("freeSegs=%u\n", a.freeSegs); */
/*     printf("validateBlock 1 = %d\n", validateBlock_(b)); */
/*     printf("validateBlock 2 = %d\n", validateBlock_(b2)); */
/* #if ALLOCATOR_ALIGNED */
/*     printf("test 57 = %p, getBlock = %p, %d\n", (void*)(b->free+57), */
/*            (void*)getBlock(b->free+57), (void*)getBlock(b->free+57)==b); */
/* #endif */
/* /\*     //    showAllocator(out, &a); *\/ */
/*     showBlock(out, b); */
/*     showBlock(out, b2); */
/*     freeBlock(&a, b); */
/*     //    printf("freeSegs=%u\n", a.freeSegs); */
/*     printf("validateBlocks 1 = %d\n", validateBlocks_(b)); */
/*     freeBlock(&a, b2); */
/*     //    printf("freeSegs=%u\n", a.freeSegs); */
/*     printf("validateBlocks 2 = %d\n", validateBlocks_(b)); */
/*     showAllocator(out, &a); */
/*     printf("validateAllocator = %d\n", validateAllocator_(&a)); */
/*     Allocator_destroy(&a); */
    return 0;
}
