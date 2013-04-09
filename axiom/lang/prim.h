#ifndef axiom_lang_prim_H_
#define axiom_lang_prim_H_

#include <map>
#include <vector>

typedef void(Proc*)();
typedef void* Addr; // todo
typedef int Tag;
typedef std::vector<Instr> InstrList;
typedef std::map<Tag, InstrList> BranchMap;

enum InstrType = {
    INSTR_PUSHFRAME,
    INSTR_POPFRAME,
    INSTR_RESIZEFRAME,
    INSTR_ALLOC,
    INSTR_STORE,
    INSTR_STOREVALUE,
    INSTR_LOAD,
    INSTR_LOADVALUE,
    INSTR_BRANCH,
    INSTR_CALL,
};

struct Instr {
    InstrType type;
    union {
        PushFrameInstr push;
        PopFrameInstr pop;
        ResizeFrameInstr resize;
        AllocInstr alloc;
        StoreInstr store;
        StoreValueInstr storeVal;
        LoadInstr load;
        LoadValueInstr loadVal;
        BranchInstr branch;
        CallInstr call;
        ReturnInstr ret;
    };
};

struct PushFrameInstr {
    size_t sz;
};

struct PopFrameInstr {
    //    size_t sz;
};

struct ResizeFrameInstr {
    size_t sz;
};

struct AllocInstr {
    size_t sz, dest;
};

struct StoreInstr {
    size_t sz, src;
    Addr dest;
};

struct StoreValueInstr {
    Addr dest;
    int val;
};

struct LoadInstr {
    size_t sz, dest;
    Addr src;
};

struct LoadValueInstr { // todo: support any literal type
    size_t dest;
    int val;
    //    size_t sz, src, dest;
};

struct BranchInstr {
    BranchMap alts;
    InstrList defAlt;
    size_t src;
};

struct CallInstr {
    Proc proc;
    bool tailCall;
};

struct ReturnInstr {
};

#endif
