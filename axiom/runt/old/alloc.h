#ifndef axiom_runt_alloc_H_
#define axiom_runt_alloc_H_

#include "../platform/types.h"

// managed memory allocation interface

namespace runt {

    struct AllocContext; // hides allocator details
    AllocContext* getAllocContext();

    void allocInit();
    void allocQuit();
    // change return type to some kind of closure ptr? word* is probably right
    Word* alloc(AllocContext* context, unsigned nWords);
}

#endif
