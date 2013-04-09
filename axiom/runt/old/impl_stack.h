#ifndef axiom_runt_impl_stack_H_
#define axiom_runt_impl_stack_H_

#import "stack.h"

namespace runt {
    // have to track ptrs into the heap for garbage collection
    struct FrameHeader {
        Word payloadSize, numPtrs; 
    };

    // size in words is most useful
    const Word FRAME_HEADER_SIZE = sizeof(FrameHeader)/sizeof(Word);

    inline WordPtr framePayload(const FrameHeader& f) {
        return ((WordPtr)&f)+FRAME_HEADER_SIZE;
    }

    inline WordPtr stackTop(const Stack& stack) {
        return stack.cur-FRAME_HEADER_SIZE;
    }

    inline FrameHeader& topFrame(const Stack& stack) {
        return *((FrameHeader*)stackTop(stack));
    }

    inline FrameHeader& prevFrame(const FrameHeader& f) {
        return *((FrameHeader*)(((WordPtr)&f)+f.payloadSize+FRAME_HEADER_SIZE));
    }

    inline bool isBottomFrame(const Stack& stack, const FrameHeader& f) {
        return ((WordPtr)&f) == stack.limit;
    }

    inline Word stackTotal(const Stack& stack) {
        return stack.limit-stack.base;
    }

    inline Word stackUsed(const Stack& stack) {
        return stack.limit-stack.cur + FRAME_HEADER_SIZE;
    }

    inline Word stackUnused(const Stack& stack) {
        return stack.cur-stack.base - FRAME_HEADER_SIZE;
    }

    template <typename OutStream>
    void showFrame(OutStream& out, const FrameHeader& f) {
        WordPtr payload = framePayload(f);
        WordPtr valsBegin = payload + f.numPtrs;
        WordPtr end = payload + f.payloadSize;
        WordPtr cur = payload;
        out << "payload size: " << f.payloadSize << '\n';
        out << f.numPtrs << " pointers: " << '\n';
        for (; cur != valsBegin; ++cur)
            out << cur-payload << ":" << *cur << ' ';
        out << '\n' << f.payloadSize - f.numPtrs << " values: " << '\n';
        for (; cur != end; ++cur)
            out << cur-payload << ":" << *cur << ' ';
        out << '\n';
    }

    template <typename OutStream>
    void showStack(OutStream& out, const Stack& stack) {
        out << "stack base: " << stack.base << ", "
            << "total words: " << stackTotal(stack) << ", "
            << "used words: " << stackUsed(stack) << '\n'
            << "frame header size: " << FRAME_HEADER_SIZE << '\n';
        const FrameHeader* cur = &topFrame(stack);
        for (int frameNum=0; !isBottomFrame(stack, *cur);
             cur = &prevFrame(*cur), ++frameNum) {
            Word wordOffset = (Word*)cur - stack.base;
            out << "--------------------------------\n";
            out << "frame: " << frameNum << " at word: " << wordOffset << '\n';
            showFrame(out, *cur);
        }
    }
}

#endif
