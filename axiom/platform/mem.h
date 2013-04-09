#ifndef axiom_platform_mem_H_
#define axiom_platform_mem_H_

#include "types.h"

namespace platform {
    namespace mem {
        void init();
        void quit();
        void* newBlock(Word nBytes); // general case
        // try to extend an allocation in-place for a larger contiguous chunk
        void* extendBlock(void* p, Word nBytes);
        void freeBlock(void* p);
    }
}

#endif
