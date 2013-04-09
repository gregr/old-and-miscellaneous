#ifndef axiom_runt_closure_H_
#define axiom_runt_closure_H_

#include "../platform/types.h"

namespace runt {

    typedef platform::Word Word
    typedef platform::WordPtr WordPtr
    typedef platform::HalfWord HalfWord

    typedef void*(*ContPtr)();
    typedef ContPtr(*Cont)();
#define JUMP(cont) return((ContPtr)(cont))

    typedef Cont(*FuncPtr)(); // varargs...

    struct Info {
        Cont entry; //, evac, scav;
    };

    struct FuncInfo {
        Cont entry;
        FuncPtr call, slowCall;
        // layout
    };

    struct Header {
        const Info* info;
        // misc
    };

    struct Closure {
        Header header;
    };

    struct Indirect {
        Header header;
        Closure* target;
    };

    struct PartialApp {
        Header header;
        const Closure* func; // func should already know boxed vs. unboxed args
        HalfWord arity, nargs;
    };

//     struct ClosureConstr {
//         Header header;
//         //        Name constr;
//     };

    // let(rec)local
    struct LocalFrame { // for allocating closures on the stack (no escaping)
        Header header;
        // layout
        // stack closures
    };

    struct CallFrame {
        Header header; // arg layout
        // args
    };

    struct ReturnFrame {
        Header header;
        // free vars
    };

    struct UpdateFrame { // placed on stack B to simulate saving return stack
        Header header;
        Closure* closure;
        // unnecessary with call frames
        //        WordPtr baseA, baseB; // simulate saving argument stacks
    };

// Eval* closures/frames used for interpreted/dynamic code

    // todo: define instruction types for each expr
    // struct EvalInstr {};

    struct EvalApp { // args!
        Header header;
        HalfWord arity, size;
        // arity layout?
    };

    struct EvalThunk { // no args
        Header header;
        const EvalInstr* instr;
    };

    struct Evaluation {
        Header header;
        Closure* cur;
        const EvalInstr* curInstr;
        // for backtracking lets (allocs and free var assignments)
        // actually this reversing should be done when compiling...
        const EvalInstr* allocInstrs;
        // arity, size
        // layout
        // need to allocate managed arrays to allow for gc? or not?
        //        Ptr instructions; // use vector if not? (well, how do you finalize?)
    };
    // instructions:
    // let, letrec, letlocal, letreclocal, case, app, constr, var, prim, primop
    // var could point to a thunk, to an evaluated closure, or be primitive...

#define ENTER(closure) JUMP(closure.header.info->entry)
}

#endif
