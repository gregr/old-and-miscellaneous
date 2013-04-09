#ifndef atom_lang_expr_H_
#define atom_lang_expr_H_

#include <string>
#include <vector>
#include "value.h"

namespace lang {

    typedef std::vector<Value> ValueList;

    // todo: Name should move somewhere common (used by env as well)
    typedef std::string Name; // hide what a name really is
    typedef std::vector<Name> NameList;

    enum UpdateFlag { UF_Yes, UF_No };

    struct Binding {
        const Expr* expr;
        NameList freeVars, boundVars;
        Name name;
        UpdateFlag uf;
    };

    typedef std::vector<Binding> BindList;

    struct Atom {
        bool literal;
        union {
            Name var;
            Value lit; // just don't use addr
        };
    };

    typedef std::vector<Atom> AtomList;

    enum AltType { ALT_ALG, ALT_PRIM, ALT_VAR, ALT_EMPTY };

    struct Expr;

    struct AlgAlt {
        NameList vars;
        Name constr;
        const Expr* expr;
    };

    struct PrimAlt {
        Value val;
        const Expr* expr;
    };

    struct VarAlt {
        Name name;
        const Expr* expr;
    };

    struct EmptyAlt {
        const Expr* expr;
    };

    struct Alt {
        AltType type;
        union {
            AlgAlt alg;
            PrimAlt prim;
            VarAlt var;
            EmptyAlt empty;
        };
    };

    typedef std::vector<Alt> AltList;

    enum ExprType { EXPR_LET, EXPR_LETREC, EXPR_CASE, EXPR_APP, EXPR_CONSTR,
                    EXPR_CONSTANT, EXPR_PRIMOP };

    struct LetExpr {
        BindList binds;
        const Expr* expr;
    };

    struct LetRecExpr {
        BindList binds;
        const Expr* expr;
    };

    struct CaseExpr {
        AltList alts;
        const Expr* expr;
    };

    struct AppExpr {
        AtomList args;
        Name var;
    };

    struct ConstrExpr {
        AtomList args;
        Name constr;
    };

    struct ConstantExpr {
        Value lit;
    };

    struct PrimOpExpr {
        AtomList args;
        Name op;
    };

    struct Expr {
        ExprType type;
        union {
            LetExpr let;
            LetRecExpr letrec;
            CaseExpr cas;
            AppExpr app;
            ConstrExpr constr;
            ConstantExpr constant;
            PrimOpExpr primop;
        }
    };
}

#endif
