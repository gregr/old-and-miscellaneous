#include "../platform/mem.h"
using namespace platform;

namespace runt {

    struct AllocContext {
    };

    static AllocContext theContext; // todo: obviously this is just temporary

    AllocContext* getAllocContext() { return &theContext; } // todo: as is this

    void allocInit() {
        memInit();
    }

    void allocQuit() {
        memQuit();
    }

    Word* alloc(AllocContext* context, unsigned nWords) {
        return (Word*)memNew(sizeof(Word)*nWords); // cheat for now
    }
}
