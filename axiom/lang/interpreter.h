#ifndef axiom_lang_interpreter_H_
#define axiom_lang_interpreter_H_

#include "expr.h"
#include "../runt/state.h"

namespace lang {

    unsigned closureSize(const Bind& bind) { // todo
        return 0;
    }

    void allocBind(Word* dest, const Bind& bind, const Env& env) {
        ValueList freeVals;
        NameList::const_iterator itr = bind.freeVars.begin(),
            end = bind.freeVars.end();
        for (; itr != end; ++itr) {
            freeVals.push_back(env.get(*itr)); // maybe unnecessary
        }
        switch (bind.expr->type) {
            case EXPR_LET:
                break;
            case EXPR_LETREC:
                break;
            case EXPR_CASE:
                break;
            case EXPR_APP:
                break;
            case EXPR_CONSTR:
                break;
            case EXPR_CONSTANT:
                break;
            case EXPR_PRIMOP:
                break;
            default:
        }
    }

    void checkHeap(State& state, unsigned allocSize) {
        if (state.heapLim - state.heap < allocSize) {
            // todo: perform garbage collection
        }
    }

    void satisfyHeap(State& state, const BindList& binds) {
        unsigned total = 0;
        BindList::const_iterator itr = binds.begin(), end = binds.end();
        for (; itr != end; ++itr)
            total += closureSize(*itr);
        checkHeap(state, total);
    }

    // target and reference could be the same, as in letrec
    void allocBinds(Word* dest, const BindList& binds, Env& targetEnv,
                    const Env& refEnv) {
        unsigned offset = 0;
        BindList::const_iterator itr = binds.begin(), end = binds.end();
        for (; itr != end; ++itr) { // allow for mutually recursive binds
            targetEnv.add(itr->name, dest+offset);
            offset += closureSize(*itr);
        }
        itr = binds.begin();
        for (; itr != end; ++itr) { // actual allocation
            allocBind(dest, *itr, refEnv);
            dest += closureSize(*itr);
        }
    }

    void letBinds(State& state, const BindList& binds, Env& newEnv) {
        satisfyHeap(state, binds);
        allocBinds(state.heap, binds, env, env.parent);
    }

    void letrecBinds(State& state, const BindList& binds, Env& env) {
        satisfyHeap(state, binds);
        allocBinds(state.heap, binds, env, env);
    }

    void eval(State& state, const Expr& expr, Env& env) {
        switch (expr.type) {
        }
    }

    State& getState() {} // global for now, thread-local later

    ContPtr evalExpr() {
    }

    struct ReturnFrame {
        AltList alts;
        Env;
    };

    ContPtr returnPrim() {
        getState();
    }

    ContPtr returnConstr() {
    }

    void halt(Value v) { throw v; }

    Value interpret(State& state, const Expr& expr) {
        try {
            //            exec();
        } catch (const Value& v) {
            return v;
        }
    }
}

#endif
