#include <stdlib.h> // for malloc

namespace platform {
    namespace mem {

        void init() {} // does nothing for now
        void quit() {}

        // todo: mmap and munmap to support extension interface
        void* newBlock(unsigned nBytes) {
            return malloc(nBytes); // for now
        }

        // hopefully can reduce fragmentation in the future
        void* extendBlock(void* p, unsigned nBytes) {
            return 0; // until implemented
        }

        void freeBlock(void* p) {
            free(p);
        }
    }
}
