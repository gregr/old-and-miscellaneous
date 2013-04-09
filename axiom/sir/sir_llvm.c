#include "sir_llvm.h"

static void sir_Env_destroy(Env* e) {
    Env_destroyData(e, &free);
    Env_destroy(e);
}

void sir_llvm_CompiledMod_init(sir_llvm_CompiledMod* cm,
                               LLVMModuleRef m, int ownsMod) {
    Env_init(&cm->bindings, NULL);
    cm->mod = m;
    cm->ownsMod = ownsMod;
}

void sir_llvm_CompiledMod_destroy(sir_llvm_CompiledMod* cm) {
    sir_Env_destroy(&cm->bindings);
    if (cm->ownsMod) LLVMDisposeModule(cm->mod);
}

void sir_llvm_ExecState_init(sir_llvm_ExecState* es, LLVMModuleRef m,
                             char** error) {
    sir_llvm_CompiledMod_init(&es->cmod, m, 0);
    es->mp = LLVMCreateModuleProviderForExistingModule(m);
    LLVMCreateExecutionEngine(&es->ee, es->mp, error);
}

void sir_llvm_ExecState_destroy(sir_llvm_ExecState* es) {
    sir_llvm_CompiledMod_destroy(&es->cmod);
    LLVMDisposeExecutionEngine(es->ee);
}

////////////////////////////////////////////////////////////////
// Linking

/* void sir_llvm_linkMod(LLVMModuleRef, const LLVMModuleRef); */
/* void sir_llvm_linkFile(LLVMModuleRef, const char*); */

////////////////////////////////////////////////////////////////
// Code Emission

/* void sir_llvm_emitBC(const LLVMModuleRef, FILE*); */
/* void sir_llvm_emitLL(const LLVMModuleRef, FILE*); */

////////////////////////////////////////////////////////////////
// Compilation

typedef struct {
    const sir_Type* t;
    LLVMValueRef v;
} CTV;

typedef struct {
    Env bindings;
    LLVMBuilderRef builder;
    LLVMModuleRef mod;
} sir_llvm_ProcEnv;

static void sir_llvm_ProcEnv_init(sir_llvm_ProcEnv* pe, sir_llvm_CompiledMod* cm) {
    pe->builder = LLVMCreateBuilder();
    Env_init(&pe->bindings, &cm->bindings);
    pe->mod = cm->mod;
}

static void sir_llvm_ProcEnv_destroy(sir_llvm_ProcEnv* pe) {
    sir_Env_destroy(&pe->bindings);
    LLVMDisposeBuilder(pe->builder);
}

// caller frees
static LLVMTypeRef* llvmTypes(const sir_TypeList*);

static LLVMTypeRef llvmType(const sir_Type* t) {
    switch (t->type) {
        case T_VOID: return LLVMVoidType();
        case T_INT: case T_UINT: return LLVMIntType(t->u.bits);
        case T_FLOAT:
            if (t->u.bits <= 32) return LLVMFloatType();
            else return LLVMDoubleType();
        case T_PTR: return LLVMPointerType(llvmType(t->u.subType), 0);
        case T_ARRAY: return LLVMArrayType(llvmType(t->u.arr->subType),
                                           t->u.arr->size);
        case T_STRUCT: {
            size_t count = ARRAY_size(*(t->u.strct->subTypes));
            LLVMTypeRef* ts = llvmTypes(t->u.strct->subTypes);
            LLVMTypeRef tr = LLVMStructType(ts, count, t->u.strct->packed);
            free(ts);
            return tr;
        }
        case T_PROC: {
            size_t count = ARRAY_size(*(t->u.proc->paramTypes));
            LLVMTypeRef* ts = llvmTypes(t->u.proc->paramTypes);
            LLVMTypeRef tr = LLVMFunctionType(llvmType(t->u.proc->retType),
                                              ts, count, 0);
            free(ts);
            return tr;
        }
        default: return NULL;//throw logic_error("undefined llvmType conversion");
    }
}

static LLVMTypeRef* llvmTypes(const sir_TypeList* ts) {
    LLVMTypeRef *result, *itr;
    MALLOC(result, sizeof(LLVMTypeRef)*ARRAY_size(*ts));
    itr = result;
    ARRAY_foreach(sir_Type*, *ts, {*itr = llvmType(*x); ++itr;});
    return result;
}

// caller frees
static LLVMValueRef* llvmValues(LLVMModuleRef, const sir_ValueList*, size_t);

static LLVMValueRef llvmValue(LLVMModuleRef mod, const sir_Value* v) {
    switch (v->type->type) {
        case T_PTR: return LLVMConstNull(llvmType(v->type));
        case T_INT: return LLVMConstInt(llvmType(v->type), v->u.i, 1);
        case T_UINT: return LLVMConstInt(llvmType(v->type), v->u.i, 0);
        case T_FLOAT: {
            double d;
            if (v->type->u.bits > 32) d = v->u.d; else d = v->u.f;
            return LLVMConstReal(llvmType(v->type), d);
        }
        case T_ARRAY: {
            size_t sz = v->type->u.arr->size;
            size_t nUndef = sz - ARRAY_size(*v->u.arr->elements);
            LLVMValueRef* vs = llvmValues(mod, v->u.arr->elements, nUndef);
            LLVMTypeRef subType = llvmType(v->type->u.arr->subType);
            LLVMValueRef *itr = vs+sz-nUndef, *end = vs+sz;
            for (; itr != end; ++itr) *itr = LLVMGetUndef(subType);
            LLVMValueRef vr = LLVMConstArray(subType, vs, sz);
            free(vs);
            return vr;
        }
        case T_STRUCT: {
            size_t sz = ARRAY_size(*v->type->u.strct->subTypes);
            size_t nUndef = sz - ARRAY_size(*v->u.strct->fields);
            LLVMValueRef* vs = llvmValues(mod, v->u.strct->fields, nUndef);
            LLVMValueRef *itr = vs+sz-nUndef, *end = vs+sz;
            sir_Type** curField = v->type->u.strct->subTypes->begin;
            for (; itr != end; ++itr, ++curField)
                *itr = LLVMGetUndef(llvmType(*curField));
            LLVMValueRef vr = LLVMConstStruct(vs, sz, v->type->u.strct->packed);
            free(vs);
            return vr;
        }
        case T_PROC: return LLVMGetNamedFunction(mod, v->u.proc->name->sym);
        default: return NULL;// throw logic_error("invalid val type");
    }
}

static LLVMValueRef* llvmValues(LLVMModuleRef mod, const sir_ValueList* vs,
                                size_t extra) {
    LLVMValueRef *result, *itr;
    MALLOC(result, sizeof(LLVMValueRef)*(ARRAY_size(*vs)+extra));
    itr = result;
    ARRAY_foreach(sir_Value*, *vs, {*itr = llvmValue(mod, *x); ++itr;});
    return result;
}

static const sir_Type* sir_llvm_getType(void* d) {
    return ((CTV*)d)->t;
}

static const sir_Type* sir_llvm_exprType(const Env* env, const sir_Expr* e) {
    return exprType(env, &sir_llvm_getType, e);
}

static LLVMValueRef compileExpr(sir_llvm_ProcEnv*, const sir_Expr*);

static inline CTV compileExprTyped(sir_llvm_ProcEnv* env, const sir_Expr* e) {
    CTV v;
    v.v = compileExpr(env, e);
    v.t = sir_llvm_exprType(&env->bindings, e);
    return v;
}

static LLVMValueRef compileReturn(sir_llvm_ProcEnv* env,
                                  const sir_ReturnExpr* r) {
    if (r->val != NULL){
        CTV ret = compileExprTyped(env, r->val);
        if (ret.t == NULL || ret.t->type == T_VOID)
            return LLVMBuildRetVoid(env->builder);
        return LLVMBuildRet(env->builder, ret.v);
    } else return LLVMBuildRetVoid(env->builder);
}

static LLVMValueRef compileExpr(sir_llvm_ProcEnv* env, const sir_Expr* e) {
    switch (e->type) {
        case EXPR_Const: return llvmValue(env->mod, &e->u.cnst->val);
            // todo: assert name in bindings?
        case EXPR_Var: return Env_get(&env->bindings, e->u.var->name);
        case EXPR_Assign: {
            const sir_AssignExpr* a = e->u.assign;
            CTV *v, v_;
            v_ = compileExprTyped(env, a->rhs);
            MALLOC(v, sizeof(CTV));
            *v = v_;
            Env_add(&env->bindings, a->lhs, v);
            return NULL;
        }
/*         case EXPR_Case: cv.v = compileCase(env, *e.cse); break; */
/*         case EXPR_Cast: cv.v = compileCast(env, *e.cast); break; */
/*         case EXPR_Op: cv.v = compileOp(env, *e.op); break; */
/*         case EXPR_Call: cv.v = compileCall(env, *e.call); break; */
        case EXPR_Return: return compileReturn(env, e->u.ret);
/*         case EXPR_TypeAttr: cv.v = compileTypeAttr(env, *e.tattr); break; */
        default: return NULL;//throw logic_error("invalid expr type");
    }
    //    return cv; //make_pair(exprType<llvm::Value*>(env.bindings, e), v);
}

/* LLVMValueRef sir_llvm_compileProcSig(sir_llvm_CompiledMod* cm, */
/*                                      const sir_ProcSig* sig) { */
/*     LLVMValueRef f = LLVMGetNamedFunction(cm->mod, name); */
/*     if (f == NULL) { */
/*     } else { */
/*     } */
/* } */

/* LLVMValueRef sir_llvm_compileProcDef(sir_llvm_CompiledMod*, */
/*                                      const sir_ProcDef* def) { */
/* } */

/* LLVMValueRef sir_llvm_compileGlobalSig(sir_llvm_CompiledMod* cm, */
/*                                        const sir_GlobalSig* sig) { */
/* } */

/* LLVMValueRef sir_llvm_compileGlobalDef(sir_llvm_CompiledMod* cm, */
/*                                        const sir_GlobalDef* def) { */
/* } */

////////////////////////////////////////////////////////////////
// JIT Execution

//sir_Value sir_llvm_execExpr(sir_llvm_ExecState*, const sir_Expr*);

/* todo: optimizations
  LLVMPassManagerRef pm = LLVMCreatePassManager();
  LLVMPassManagerRef fpm = LLVMCreateFunctionPassManager(mp);
  LLVMAddTargetData(LLVMGetExecutionEngineTargetData(ee), pm);
  LLVMAddConstantPropagationPass(pm);
  LLVMAddInstructionCombiningPass(pm);
  LLVMAddPromoteMemoryToRegisterPass(pm);
  LLVMAddReassociatePass(pm);
  LLVMAddGVNPass(pm);
  LLVMAddCFGSimplificationPass(pm);
  LLVMAddTailCallEliminationPass(pm);
  LLVMAddInstructionCombiningPass(pm);
  LLVMRunPassManager(pm, mod);
  LLVMDumpModule(mod);
  LLVMDisposePassManager(pm);
  LLVMDisposePassManager(fpm);
 */
