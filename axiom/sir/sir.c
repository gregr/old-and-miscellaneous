#include "sir.h"

void showValue(FILE* out, const sir_Value* v) {
    switch (v->type->type) {
        case T_VOID: break;
        case T_INT: fprintf(out, "%d", v->u.i); break;
        case T_UINT: fprintf(out, "%u", v->u.ui); break;
        case T_FLOAT:
            if (v->type->u.bits > 32) fprintf(out, "%f", v->u.d);
            else fprintf(out, "%f", v->u.f);
            break;
        default: fprintf(out, "<unshowable>");
    }
}

const sir_Type voidType = {T_VOID, {0}};
const sir_Type boolType = UINT_TYPE(1);
const sir_Type byteType = UINT_TYPE(8);
const sir_Type charType = INT_TYPE(8);
const sir_Type intType = INT_TYPE(32);
const sir_Type uintType = UINT_TYPE(32);
const sir_Type floatType = FLOAT_TYPE(32);
const sir_Type doubleType = FLOAT_TYPE(64);

static int typesEq(const sir_TypeList* ts1, const sir_TypeList* ts2) {
    size_t sz = ARRAY_size(*ts1);
    if (ARRAY_size(*ts2) == sz) {
        sir_Type **t1 = ts1->begin, **t2 = ts2->begin;
        sir_Type **t1e = ts1->end;
        for (; t1 != t1e; ++t1, ++t2) if (!typeEq(*t1, *t2)) return 0;
        return 1;
    } else return 0;
}

int typeEq(const sir_Type* t1, const sir_Type* t2) {
    if (t1 == t2) return 1;
    if (t1->type != t2->type) return 0;
    switch (t1->type) {
        case T_VOID: return 1;
        case T_INT: case T_UINT: case T_FLOAT: return t1->u.bits == t2->u.bits;
        case T_PTR: return typeEq(t1->u.subType, t2->u.subType);
        case T_ARRAY: return (t1->u.arr->size == t2->u.arr->size) &&
            typeEq(t1->u.arr->subType, t2->u.arr->subType);
        case T_STRUCT: return (t1->u.strct->packed == t2->u.strct->packed) &&
            typesEq(t1->u.strct->subTypes, t2->u.strct->subTypes);
        case T_PROC: return typeEq(t1->u.proc->retType, t2->u.proc->retType) &&
            typesEq(t1->u.proc->paramTypes, t2->u.proc->paramTypes);
        default: return 0; // this shouldn't happen... error check?
    }
}

static inline const sir_Expr* getOpArg(const sir_Expr* e, size_t i) {
    const sir_Expr* arg;
    ARRAY_get(arg, *e->u.op->args, i);
    return arg;
}

const sir_Type* aggSubtype(const sir_Type* aggType, size_t i) {
    //        const sir::Type& aggType = exprType(args.at(0));
    switch (aggType->type) {
        case T_ARRAY: return aggType->u.arr->subType;
        case T_STRUCT: {
            const sir_Type* t;
            ARRAY_get(t, *aggType->u.strct->subTypes, i);
            return t;
        }
        default: return NULL;//throw logic_error("invalid agg type");
    }
}

int validIndex(const sir_Expr* e) {
    return (e->type == EXPR_Const) && (e->u.cnst->val.type->type == T_UINT);
}

size_t indexFromExpr(const sir_Expr* e) {
    // todo: error handling
    /*         throw runtime_error("expr is not a valid constant unsigned index"); */
    return e->u.cnst->val.u.ui;
}

const sir_Type* exprType(const Env* env, const sir_Type*(*getType)(void*),
                         const sir_Expr* e) {
    switch (e->type) {
        case EXPR_Const: return e->u.cnst->val.type;
        case EXPR_Var: return (*getType)(Env_get(env, e->u.var->name));
        case EXPR_Assign: return NULL;
        case EXPR_Case: return e->u.cse->contType;
        case EXPR_Cast: return e->u.cast->target;
        case EXPR_Op:
            switch (e->u.op->type) {
                case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV:
                case OP_REM: case OP_SHL: case OP_SHR: case OP_AND:
                case OP_OR: case OP_XOR: case OP_NEG: case OP_NOT:
                case OP_INSERT: case OP_OFFSET:
                    return exprType(env, getType, getOpArg(e, 0));
                case OP_EQ: case OP_NEQ: case OP_LT: case OP_LTEQ:
                case OP_GT: case OP_GTEQ: case OP_ISNAN: return &boolType;
                case OP_STORE: return &voidType;
                    // todo: validate ptr type
                case OP_LOAD: return exprType(env, getType,
                                              getOpArg(e, 0))->u.subType;
                    // todo: validate ptr type
                case OP_SELECT:
                    return aggSubtype(exprType(env, getType,
                                               getOpArg(e, 0))->u.subType,
                                      indexFromExpr(getOpArg(e, 1)));
                case OP_EXTRACT:
                    return aggSubtype(exprType(env, getType,
                                               getOpArg(e, 0)),
                                      indexFromExpr(getOpArg(e, 1)));
            }
        case EXPR_Call:
            // todo: validate proc type
            return exprType(env, getType, e->u.call->proc)->u.proc->retType;
            //                return *e.call->proc->type->proc->retType;
        case EXPR_Return: return NULL;//throw std::logic_error("expr has no type");
        case EXPR_TypeAttr:
            switch (e->u.tattr->type) {
                case TYPEATTR_SIZEOF: return &uintType;
                default: return NULL;//throw std::logic_error("exprType of invalid TypeAttrExpr");
            };
        default: return NULL; // should be unreachable
    }
    //    throw std::logic_error("unreachable");
}

/* static void validateType(const sir_Type*); */

/* static void validateTypes(const TypeList* ts) { */
/*     TypeList::const_iterator itr = ts.begin(), end = ts.end(); */
/*     for (; itr != end; ++itr) validateType(**itr); */
/* } */

/* static int validateType(const sir_Type* t) { */
/*     switch (t->type) { */
/*         case T_VOID: */
/*         case T_INT: case T_UINT: case T_FLOAT: return 1; */
/*         case T_PTR: return validateType(t->subType); */
/*         case T_ARRAY: return validateType(t->arr->subType); */
/*         case T_STRUCT: return validateTypes(t->strct->subTypes); */
/*         case T_PROC: return validateTypes(t->proc->paramTypes); */
/*         default: return 0;//throw logic_error("unknown type type"); */
/*     } */
/* } */

/* static void validateValue(const sir_Value*, const Type* tmatch=0); */

/* static void validateValuesWithType(const ValueList& vs, const Type* t) { */
/*     ValueList::const_iterator itr = vs.begin(), end = vs.end(); */
/*     for (; itr != end; ++itr) validateValue(*itr, t); */
/* } */

/* static void validateValuesWithTypes(const ValueList& vs, const TypeList& ts) { */
/*     if (vs.size() != ts.size()) */
/*         throw logic_error("number of values differs from number of types"); */
/*     ValueList::const_iterator itr = vs.begin(), end = vs.end(); */
/*     TypeList::const_iterator t = ts.begin(); */
/*     for (; itr != end; ++t, ++itr) validateValue(*itr, *t); */
/* } */

/* static void validateValue(const Value& v, const Type* tmatch) { */
/*     validateType(*v.type); */
/*     if (tmatch != 0 && !typeEq(*v.type, *tmatch)) */
/*         // todo: print types */
/*         throw logic_error("value does not have expected type"); */
/*     switch (v.type->type) { */
/*         case T_ARRAY: */
/*             validateValuesWithType(v.arr->elements, v.type->arr->subType); */
/*         case T_STRUCT: */
/*             validateValuesWithTypes(v.strct->fields, v.type->strct->subTypes); */
/*         default: break; */
/*     } */
/* } */

/* static void validateExprs(const ExprList& es, bool recurse) { */
/*     ExprList::const_iterator itr = es.begin(), end = es.end(); */
/*     for (; itr != end; ++itr) validateExpr(**itr, recurse); */
/* } */

/* static void validateAlts(const AltList& as, bool recurse) { */
/*     AltList::const_iterator itr = as.begin(), end = as.end(); */
/*     for (; itr != end; ++itr) validateExprs(itr->second, recurse); */
/* } */

/* void validateExpr(const Expr& e, bool recurse) { // todo */
/*     switch (e.type) { */
/*         case EXPR_Const: validateValue(e.cnst->val); break; */
/*         case EXPR_Var: break; // todo? need env */
/*         case EXPR_Assign: */
/*             if (recurse) validateExpr(*e.assign->rhs, true); break; */
/*         case EXPR_Case: */
/*             validateType(*e.cse->contType); */
/*             if (recurse) { */
/*                 validateAlts(e.cse->alts, true); */
/*                 validateExprs(e.cse->defAlt, true); */
/*                 validateExpr(*e.cse->scrutinee, true); */
/*             } */
/*             break; */
/*         case EXPR_Cast: */
/*         case EXPR_Op: */
/*         case EXPR_Call: */
/*             break; */
/*         case EXPR_Return: if (recurse) validateExpr(*e.ret->val, true); */
/*             break; */
/*         case EXPR_TypeAttr: */
/*             break; */
/*         default: throw logic_error("unknown expr type"); */
/*     } */
/* } */

/* void validateProcSig(const ProcSig& s) { */
/*     if (s.type->type != T_PROC) */
/*         throw logic_error("proc signature must have proc type"); */
/*     validateType(*s.type); */
/* } */

/* void validateProcDef(const ProcDef& d, bool recurse) { // todo: validate type of returns */
/*     validateProcSig(*d.sig); */
/*     if (d.paramNames.size() != d.sig->type->proc->paramTypes.size()) */
/*         throw logic_error("mismatching types and names in proc params"); */
/*     if (recurse) validateExprs(d.body, true); */
/* } */

/* void validateGlobalSig(const GlobalSig& s) { */
/*     if (s.type->type != T_PTR) */
/*         throw logic_error("global signature must have pointer type"); */
/*     validateType(*s.type); */
/* } */

/* void validateGlobalDef(const GlobalDef& d) { */
/*     validateGlobalSig(*d.sig); */
/*     validateValue(d.val); */
/*     if (!typeEq(*d.val.type, *d.sig->type->subType)) */
/*         throw logic_error("global value does not match its type"); */
/* } */
