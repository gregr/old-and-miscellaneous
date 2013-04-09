#ifndef axiom_sir_H_
#define axiom_sir_H_

#include "../common/env.h"
#include <stdio.h> // todo

typedef enum { T_VOID, T_INT, T_UINT, T_FLOAT,
               T_PTR, T_ARRAY, T_STRUCT, T_PROC
} sir_TypeType;

typedef enum { CAST_RESIZE, // truncate or (zero|sign)-extend
               CAST_CONVERT, // int to float, ptr to int, etc.
               CAST_REINTERP // bits remain identical, but type changes
} sir_CastType ;

typedef enum {
    // aggregate ops
    OP_SELECT, OP_EXTRACT, OP_INSERT,
    // unary
    OP_NEG, OP_NOT, OP_ISNAN, OP_LOAD,
    // binary
    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_REM, OP_SHL, OP_SHR,
    OP_AND, OP_OR, OP_XOR,
    OP_EQ, OP_NEQ, OP_LT, OP_LTEQ, OP_GT, OP_GTEQ,
    OP_OFFSET, OP_STORE
    // ternary
} sir_OpType;

typedef enum { TYPEATTR_SIZEOF } sir_TypeAttrType;

typedef struct sir_Type_ {
    sir_TypeType type;
    union {
        const struct sir_Type_* subType;
        const struct sir_ArrayType_* arr;
        const struct sir_StructType_* strct;
        const struct sir_ProcType_* proc;
        size_t bits;
    } u;
} sir_Type;

int typeEq(const sir_Type* t1, const sir_Type* t2);

#define BITS_TYPE(type, nBits) {type, {.bits=nBits}}
#define UINT_TYPE(bits) BITS_TYPE(T_UINT, bits)
#define INT_TYPE(bits) BITS_TYPE(T_INT, bits)
#define FLOAT_TYPE(bits) BITS_TYPE(T_FLOAT, bits)

extern const sir_Type voidType;
extern const sir_Type boolType;
extern const sir_Type byteType;
extern const sir_Type charType;
extern const sir_Type intType;
extern const sir_Type uintType;
extern const sir_Type floatType;
extern const sir_Type doubleType;

#define DEF_LIST(ty, name) ARRAY_STRUCT(ty, sir_##name##List)

DEF_LIST(sir_Type*, Type);

typedef struct sir_ArrayType_ {
    const sir_Type* subType;
    size_t size;
} sir_ArrayType;

typedef struct sir_StructType_ {
    const sir_TypeList* subTypes;
    int packed;
} sir_StructType;

typedef struct sir_ProcType_ {
    const sir_TypeList* paramTypes;
    const sir_Type* retType;
} sir_ProcType;

typedef struct sir_Value_ {
    const sir_Type* type;
    union {
        double d;
        float f;
        int i;
        size_t ui;
        const struct sir_ArrayValue_* arr;
        const struct sir_StructValue_* strct;
        const struct sir_ProcValue_* proc;
    } u;
} sir_Value;

void sir_showValue(FILE*, const sir_Value*);

DEF_LIST(sir_Value*, Value);

typedef struct sir_ArrayValue_ {
    const sir_ValueList* elements;
} sir_ArrayValue;

typedef struct sir_StructValue_ {
    const sir_ValueList* fields;
} sir_StructValue;

typedef EnvName* sir_Name;
DEF_LIST(sir_Name, Name);

typedef struct sir_ProcValue_ {
    sir_Name name;
} sir_ProcValue;

typedef enum {
    EXPR_Const, EXPR_Var, EXPR_Assign, EXPR_Case,
    EXPR_Cast, EXPR_Op, EXPR_Call, EXPR_Return, EXPR_TypeAttr
} sir_ExprType;

typedef struct sir_Expr_ {
    sir_ExprType type;
    union {
        const struct sir_ConstExpr_* cnst;
        const struct sir_VarExpr_* var;
        const struct sir_AssignExpr_* assign;
        const struct sir_CaseExpr_* cse;
        const struct sir_CastExpr_* cast;
        const struct sir_OpExpr_* op;
        const struct sir_CallExpr_* call;
        const struct sir_ReturnExpr_* ret;
        const struct sir_TypeAttrExpr_* tattr;
    } u;
} sir_Expr;

DEF_LIST(sir_Expr*, Expr);

typedef struct sir_Alt_ {
    int tag;
    const sir_ExprList* contExpr;
} sir_Alt;

DEF_LIST(sir_Alt, Alt);

typedef enum { LINK_External, LINK_Internal } sir_LinkType;

typedef struct sir_ProcSig_ {
    sir_Name name;
    const sir_Type* type;
    sir_LinkType link;
} sir_ProcSig;

typedef struct sir_ProcDef_ {
    const sir_NameList* paramNames;
    const sir_ExprList* body;
    const sir_ProcSig* sig;
} sir_ProcDef;

static inline int validProcDefParams(const sir_ProcDef* d) {
    return ARRAY_size(*d->paramNames) ==
        ARRAY_size(*d->sig->type->u.proc->paramTypes);
}

typedef struct sir_GlobalSig_ {
    sir_Name name;
    const sir_Type* type;
    sir_LinkType link;
    int constant, threadLocal;
} sir_GlobalSig;

typedef struct sir_GlobalDef_ {
    sir_Value val;
    const sir_GlobalSig* sig;
} sir_GlobalDef;

typedef struct sir_ConstExpr_ {
    sir_Value val;
} sir_ConstExpr;

typedef struct sir_VarExpr_ {
    sir_Name name;
} sir_VarExpr;

typedef struct sir_AssignExpr_ {
    sir_Name lhs;
    const sir_Expr* rhs;
} sir_AssignExpr;

typedef struct sir_CaseExpr_ {
    const sir_AltList* alts;
    const sir_ExprList* defAlt;
    const sir_Type* contType;
    const sir_Expr* scrutinee;
} sir_CaseExpr;

typedef struct sir_CastExpr_ {
    sir_CastType type;
    const sir_Type* target;
    const sir_Expr* val;
} sir_CastExpr;

typedef struct sir_OpExpr_ {
    const sir_ExprList* args;
    sir_OpType type;
} sir_OpExpr;

typedef struct sir_CallExpr_ {
    const sir_ExprList* args;
    const sir_Expr* proc;
    //        const ProcSig* proc;
} sir_CallExpr;

typedef struct sir_ReturnExpr_ {
    const sir_Expr* val;
} sir_ReturnExpr;

typedef struct sir_TypeAttrExpr_ {
    sir_TypeAttrType type;
    const sir_Type* tval;
} sir_TypeAttrExpr;

// template <typename ValType>
// struct TypedEnv {
//     typedef std::pair<const sir_Type*, ValType> V;
//     typedef Env<Name, V> T;
// };

// void validateExpr(const sir_Expr*, int recurse);
// void validateProcSig(const sir_ProcSig*);
// void validateProcDef(const sir_ProcDef*, int recurse);
// void validateGlobalSig(const sir_GlobalSig*);
// void validateGlobalDef(const sir_GlobalDef*);

int validIndex(const sir_Expr*);
size_t indexFromExpr(const sir_Expr*);
// const sir_Type* aggSubtype(const sir_Type*, unsigned);

const sir_Type* exprType(const Env*, const sir_Type*(*)(void*),
                         const sir_Expr*);

// template <typename ValType>
// const sir_Type* exprType(const typename TypedEnv<ValType>::T* env,
//                      const Expr* e) {
//     switch (e.type) {
//         case EXPR_Const: return e.cnst->val.type;
//         case EXPR_Var: return env.get(e.var->name).first;
//         case EXPR_Assign: return (const sir_Type*)0;//throw std::logic_error("expr has no type");
//         case EXPR_Case: return e.cse->contType;
//         case EXPR_Cast: return e.cast->target;
//         case EXPR_Op:
//             switch (e.op->type) {
//                 case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV:
//                 case OP_REM: case OP_SHL: case OP_SHR: case OP_AND:
//                 case OP_OR: case OP_XOR: case OP_NEG: case OP_NOT:
//                 case OP_INSERT: case OP_OFFSET:
//                     return exprType<ValType>(env, *e.op->args.at(0));
//                 case OP_EQ: case OP_NEQ: case OP_LT: case OP_LTEQ:
//                 case OP_GT: case OP_GTEQ: case OP_ISNAN:
//                     return &boolType;
//                 case OP_STORE: return &voidType;
//                     // todo: validate ptr type
//                 case OP_LOAD: return exprType<ValType>(env,
//                                                        *e.op->args.at(0))->subType;
//                     // todo: validate ptr type
//                 case OP_SELECT: return aggSubtype(*exprType<ValType>(env,
//                                                                      *e.op->args.at(0))->subType,
//                                                   indexFromExpr(*e.op->args.at(1)));
//                 case OP_EXTRACT: return aggSubtype(*exprType<ValType>(env,
//                                                                       *e.op->args.at(0)),
//                                                    indexFromExpr(*e.op->args.at(1)));
//             }
//         case EXPR_Call:
//             // todo: validate proc type
//             return exprType<ValType>(env, *e.call->proc)->proc->retType;
//             //                return *e.call->proc->type->proc->retType;
//         case EXPR_Return: return (const sir_Type*)0;//throw std::logic_error("expr has no type");
//         case EXPR_TypeAttr:
//             switch (e.tattr->type) {
//                 case TYPEATTR_SIZEOF: return &uintType;
//                 default: throw std::logic_error("exprType of invalid TypeAttrExpr");
//             };
//     }
//     throw std::logic_error("unreachable");
// }

#endif
