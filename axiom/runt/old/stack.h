#ifndef axiom_runt_stack_H_
#define axiom_runt_stack_H_

#import "types.h"

namespace runt {

    // popped becomes valid after popFrame and invalidated by further stack ops
    struct Stack {
        WordPtr base, limit, cur, popped;
    };

    // stack layout
    // base               popped         cur                       limit
    // |                  |              |                         |
    // |...empty...|header|payload|header|payload|...prev frames...|
    //              popped frame   top frame

    void newStack(Stack&, Word numWords);
    void freeStack(Stack&);
    void pushFrame(Stack&, Word payloadSize, Word numPtrs);
    void popFrame(Stack&);
    // for tail calls and possibly other forms of stack consolidation
    void resizeFrame(Stack&, Word payloadSize, Word numPtrs);
}

#endif
