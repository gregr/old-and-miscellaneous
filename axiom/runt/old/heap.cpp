#include "heap.h"
using namespace platform;

namespace runt {

    void growHeap(Heap& h, Word additionalWords) {
        // todo: gc and/or additional allocation
    }

    WordPtr allocNode(Heap& h, Word numWords) {
        Word avail = h.limit - h.next;
        if (avail < numWords)
            growHeap(h, numWords - avail);
        WordPtr node = h.next;
        h.next += numWords;
        return node;
    }
}
