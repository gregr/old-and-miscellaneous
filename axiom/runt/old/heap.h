#ifndef axiom_runt_heap_H_
#define axiom_runt_heap_H_

#include "types.h"

namespace runt {

    struct Heap {
        WordPtr base, limit, next;
    };

    void growHeap(Heap&, Word additionalWords);
    WordPtr allocNode(Heap&, Word numWords);
}

#endif
