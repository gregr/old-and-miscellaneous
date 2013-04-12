#ifndef axiom_runt_state_H_
#define axiom_runt_state_H_

#include "../platform/types.h"

namespace runt {

    struct State {
        Word* heapBase, heapLim, hp, stackBase, stackLim, sp;
    };
}

#endif
