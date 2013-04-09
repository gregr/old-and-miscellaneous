#include "impl_stack.h"
#include <string.h>
using namespace platform;

namespace runt {

    // todo: use real allocator
    static WordPtr allocWords(Word numWords) {
        return new Word[numWords];
    }

    static void freeWords(WordPtr data) {
        delete[] data;
    }

    static void seatStack(Stack& s, WordPtr base, Word nWords, Word curOffset) {
        // note: s.popped is invalidated
        s.base = base;
        s.limit = base+nWords;
        s.cur = base+curOffset;
    }

    static void growStack(Stack& stack, Word additionalWords) {
        // todo: allow for some extra room
        Word newSize = stackTotal(stack)+additionalWords;
        WordPtr data = allocWords(newSize);
        memcpy(data, stackTop(stack), stackTotal(stack));
        freeWords(stack.base);
        seatStack(stack, data, newSize, stack.cur-stack.base+additionalWords);
    }

    static void maybeGrowStack(Stack& stack, Word additionalWords) {
        Word unused = stackUnused(stack);
        if (unused < additionalWords)
            growStack(stack, additionalWords - unused);
    }

    static FrameHeader& nextFrame(Stack& stack, Word payloadSize) {
        Word nextSize = FRAME_HEADER_SIZE + payloadSize;
        maybeGrowStack(stack, nextSize);
        FrameHeader& next = *((FrameHeader*)(stackTop(stack)-nextSize));
        return next;
    }

    void newStack(Stack& stack, Word numWords) {
        WordPtr data = allocWords(numWords);
        seatStack(stack, data, numWords, numWords+FRAME_HEADER_SIZE);
    }

    void freeStack(Stack& stack) {
        freeWords(stack.base);
        // todo: what should really happen here?
        stack.base = 0;
        stack.limit = 0;
        stack.cur = 0;
        stack.popped = 0;
    }

    void pushFrame(Stack& stack, Word payloadSize, Word numPtrs) {
        FrameHeader& next = nextFrame(stack, payloadSize);
        next.payloadSize = payloadSize;
        next.numPtrs = numPtrs;
        stack.cur = framePayload(next);
    }

    void popFrame(Stack& stack) {
        const FrameHeader& f = topFrame(stack);
        stack.popped = stack.cur;
        stack.cur += f.payloadSize+FRAME_HEADER_SIZE;
    }

    void resizeFrame(Stack& stack, Word payloadSize, Word numPtrs) {
        signed diff = payloadSize - topFrame(stack).payloadSize;
        if (diff > 0)
            maybeGrowStack(stack, diff);
        FrameHeader& f = *((FrameHeader*)(stackTop(stack) - diff));
        f.payloadSize = payloadSize;
        f.numPtrs = numPtrs;
        stack.cur = framePayload(f);
    }
}
