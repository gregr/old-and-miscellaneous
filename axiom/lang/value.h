#ifndef axiom_lang_value_H_
#define axiom_lang_value_H_

#include "../platform/types.h"

namespace lang {

    // should this be based on size instead? if so, what about interpretation?
    enum LitType { LIT_INT, LIT_FLOAT };
    struct Literal {
        LitType type;
        union {
            int i;
            float f;
        };
    };

    // Free = attached to current Closure
    enum AddrType = { ADDR_STATIC, ADDR_STACK, ADDR_FREE };
    struct Address {
        Address() {}
        Address(AddrType t, unsigned p) : type(t), pos(p) {}
        AddrType type;
        unsigned pos;
    };

    enum ValType { VAL_ADDR, VAL_LIT };
    struct Value {
        Value() {}
        Value(const Address& a) : type(VAL_ADDR), addr(a) {}
        Value(const Literal& l) : type(VAL_LIT), lit(l) {}
        Value(AddrType at, unsigned pos)
            : type(VAL_ADDR), addr(Address(at, pos)) {}
        ValType type;
        union {
            Address addr;
            Literal lit;
        };
    };
}

#endif
