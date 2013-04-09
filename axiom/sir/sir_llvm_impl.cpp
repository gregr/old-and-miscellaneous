#include "sir_llvm_impl.h"
//#include <llvm/DerivedTypes.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Support/IRBuilder.h>
#include <map>
#include <stdexcept>
//#include <iostream>
using namespace llvm;
using namespace std;

extern "C" {

    // todo: other way to init mod is through llvm's file loading interface
    const char* sir_llvm_Mod_init(sir_llvm_Mod* m) {
        try {
            m-> = new Module("");
        } catch (const bad_alloc&) {
        }
    }

    const char* sir_llvm_Mod_destroy(sir_llvm_Mod* m) {
    }

    const char* sir_llvm_CompiledMod_init(sir_llvm_CompiledMod* cm) {
        Env_init(&cm->bindings, NULL);
    }

    const char* sir_llvm_CompiledMod_destroy(sir_llvm_CompiledMod* cm) {
        Env_destroy(&cm->bindings);
    }

    sir_llvm_Mod* sir_llvm_CompiledMod_mod(sir_llvm_CompiledMod* cm) {
        return &cm->mod;
    }

    const char* sir_llvm_ExecState_init(sir_llvm_ExecState* es) {
        sir_llvm_CompiledMod_init(&es->cmod);
    }

    const char* sir_llvm_ExecState_destroy(sir_llvm_ExecState* es) {
        sir_llvm_CompiledMod_destroy(&es->cmod);
    }

    sir_llvm_CompiledMod* sir_llvm_ExecState_cmod(sir_llvm_ExecState* es) {
        return &es->cmod;
    }

    ////////////////////////////////////////////////////////////////
    // Linking

    const char* sir_llvm_linkMod(sir_llvm_Mod* tgt, const sir_llvm_Mod* src) {
    }

    const char* sir_llvm_linkFile(sir_llvm_Mod* tgt, const char* srcFileName) {
    }

    ////////////////////////////////////////////////////////////////
    // Code Emission

    // bc, llir, etc. use c++ stdiostream or stdio_filebuf with FILE
    // per-function versions too?
    const char* sir_llvm_emitBC(sir_llvm_Mod* m, FILE* out) {
    }

    const char* sir_llvm_emitLL(sir_llvm_Mod* m, FILE* out) {
    }

    ////////////////////////////////////////////////////////////////
    // Compilation

    const char* sir_llvm_compileProcSig(sir_llvm_CompiledMod* cm,
                                        sir_llvm_Proc* result,
                                        const sir_ProcSig* sig) {
    }

    const char* sir_llvm_compileProcDef(sir_llvm_CompiledMod* cm,
                                        sir_llvm_Proc* result,
                                        const sir_ProcDef* def) {
    }

    const char* sir_llvm_compileGlobalSig(sir_llvm_CompiledMod* cm,
                                          sir_llvm_Global* result,
                                          const sir_GlobalSig* gsig) {
    }

    const char* sir_llvm_compileGlobalDef(sir_llvm_CompiledMod* cm,
                                          sir_llvm_Global* result,
                                          const sir_GlobalDef* gdef) {
    }

    ////////////////////////////////////////////////////////////////
    // JIT Execution

    const char* sir_llvm_execExpr(sir_llvm_ExecState* es, sir_Value* result,
                                  const sir_Expr* expr) {
    }
}

struct ProcEnv {
    ProcEnv(sir_llvm_CompiledMod& cm) : mod(cm.mod.m) {
        Env_init(&bindings, &cm.bindings);
    }
    ~ProcEnv() { Env_destroy(&bindings); }
    Env bindings;
    IRBuilder<> builder;
    Module* mod;
};

/*
// most of the following is obsolete, but useful for reference
namespace sir {

    struct ProcEnv {
        ProcEnv(const LLVMCompilerEnv& cenv)
            : bindings(&cenv.globals), mod(cenv.mod) {}
        Bindings bindings;
        IRBuilder<> builder;
        Module* mod;
        //        NameMap* globals;
    };

    template <typename PassManagerType>
    static void addOptPasses(PassManagerType& pm, const ExecutionEngine& ee) {
        pm.add(new TargetData(*ee.getTargetData()));
        //        pm.add(createInstructionCombiningPass());
        pm.add(createReassociatePass());
        pm.add(createGVNPass());
        pm.add(createCFGSimplificationPass());
        pm.add(createTailCallEliminationPass());
        pm.add(createInstructionCombiningPass()); // better here?
    }

    void initExecEnv(LLVMExecEnv& exec) {
        exec.cenv.mod = new Module("");
        exec.mp = new ExistingModuleProvider(exec.cenv.mod);
        exec.ee = ExecutionEngine::create(exec.mp, false);
        addOptPasses(exec.pm, *exec.ee);
    }

    Function* getModuleProc(const Module* mod, const Name& procName) {
    //    Function* getProcByName(LLVMExecEnv& e, const Name& procName) {
        Function* f = mod->getFunction(procName);
        if (!f) throw runtime_error("unknown proc: " + procName);
        return f;
    }

    Function* getProcByName(LLVMExecEnv& e, const Name& procName) {
        return getModuleProc(e.cenv.mod, procName);
    }

    Function* execProcSig(LLVMExecEnv& e, const ProcSig& s) {
        e.cenv.fpm = 0;
        return compileProcSig(e.cenv, s);
    }

    Function* execProcDef(LLVMExecEnv& e, const ProcDef& d, bool optimize) {
        FunctionPassManager fpm(e.mp);
        if (optimize) {
            addOptPasses(fpm, *e.ee);
            e.cenv.fpm = &fpm;
        }
        else e.cenv.fpm = 0;
        return compileProcDef(e.cenv, d);
    }

//     const Val& getBound(const LLVMExecEnv& exec, const Name& name) {
//         map<Name, Val>::const_iterator
//             itr = exec.bindings.find(name);
//         if (itr == exec.bindings.end())
//             throw runtime_error("unbound var: " + name);
//         return itr->second;
//     }

    static Val valFromGeneric(const sir::Type& t, const GenericValue g) {
        Val v = {&t};
        switch (t.type) {
            case T_VOID: break;
            case T_INT: v.i = g.IntVal.getSExtValue(); break;
            case T_UINT: v.ui = g.IntVal.getZExtValue(); break;
            case T_FLOAT:
                if (t.bits > 32) v.d = g.DoubleVal;
                else v.f = g.FloatVal;
                break;
                // todo: aggregates/ptrs/functions
            default: throw runtime_error("invalid generic val");
        }
        return v;
    }

    Val execCallRaw(LLVMExecEnv& e, Function* f, const sir::Type& retType,
                    const vector<GenericValue>& args) {
        GenericValue result = e.ee->runFunction(f, args);
        return valFromGeneric(retType, result);
    }

    Val execCall(LLVMExecEnv& e, const ProcSig& sig,
                 const vector<GenericValue>& args) {
        Function* f = getProcByName(e, sig.name);
        return execCallRaw(e, f, *sig.type->proc->retType, args);
    }

    Val execExpr(LLVMExecEnv& exec, const Expr& e, bool erase) {
        const sir::Type& t = *exprType<llvm::Value*>(exec.cenv.globals, e);
        ProcType procType;
        procType.retType = &t;
        sir::Type pType = {T_PROC};
        pType.proc = &procType;
        ProcSig sig = {"", &pType, LINK_Internal};
        ProcDef def;
        def.sig = &sig;
        ReturnExpr ret = {&e};
        Expr retE = {EXPR_Return};
        retE.ret = &ret;
        def.body.push_back(&retE);
        Function* f = execProcDef(exec, def);
        vector<GenericValue> args;
        Val res = execCallRaw(exec, f, t, args);
        if (erase) f->eraseFromParent();
        return res;
    }

    static vector<const llvm::Type*> llvmTypes(const TypeList&);

    static const llvm::Type* llvmType(const sir::Type& t) {
        switch (t.type) {
            case T_VOID: return llvm::Type::VoidTy;
            case T_INT:
            case T_UINT: return IntegerType::get(t.bits);
            case T_FLOAT:
                if (t.bits <= 32) return llvm::Type::FloatTy;
                else return llvm::Type::DoubleTy;
            case T_PTR: return PointerType::get(llvmType(*t.subType), 0);
            case T_ARRAY: return llvm::ArrayType::get(llvmType(*t.arr->subType),
                                                      t.arr->size);
            case T_STRUCT: return llvm::StructType::get(
                             llvmTypes(t.strct->subTypes), t.strct->packed);
            case T_PROC: return llvm::FunctionType::get(
              llvmType(*t.proc->retType), llvmTypes(t.proc->paramTypes), false);
            default: throw logic_error("undefined llvmType conversion");
        }
    }

    static vector<const llvm::Type*> llvmTypes(const TypeList& ts) {
        vector<const llvm::Type*> out;
        out.reserve(ts.size());
        TypeList::const_iterator itr = ts.begin(), end = ts.end();
        for (; itr != end; ++itr)
            out.push_back(llvmType(**itr));
        return out;
    }

    static ConstantInt* llvmConstInt(const llvm::Type* t, int v) {
        return ConstantInt::get(t, v);
    }

    static CompiledValue compileExpr(ProcEnv&, const Expr&);

    static Constant* llvmVal(Module*, const Val&);

    static vector<Constant*> llvmVals(Module* mod, const ValList& vs) {
        vector<Constant*> out;
        ValList::const_iterator itr = vs.begin(), end = vs.end();
        for (; itr != end; ++itr)
            out.push_back(llvmVal(mod, *itr));
        return out;
    }

    static Constant* llvmVal(Module* mod, const Val& val) {
        switch (val.type->type) {
            case T_PTR:
                return ConstantPointerNull::get(
                  dynamic_cast<const llvm::PointerType*>(llvmType(*val.type)));
            case T_INT:
                return ConstantInt::get(llvmType(*val.type), val.i, true);
            case T_UINT:
                return ConstantInt::get(llvmType(*val.type), val.i, false);
            case T_FLOAT: {
                double d;
                if (val.type->bits > 32) d = val.d;
                else d = val.f;
                return ConstantFP::get(llvmType(*val.type), d);
            }
            case T_ARRAY: {
                vector<Constant*> elems = llvmVals(mod, val.arr->elements);
                const llvm::Type* subType = llvmType(*val.type->arr->subType);
                unsigned sz = val.type->arr->size;
                const llvm::ArrayType* at =
                    dynamic_cast<const llvm::ArrayType*>(llvmType(*val.type));
                for (unsigned i = val.arr->elements.size(); i < sz; ++i)
                    elems.push_back(UndefValue::get(subType));
                if (at == 0) throw runtime_error("invalid array type");
                return ConstantArray::get(at, elems);
            }
            case T_STRUCT: {
                vector<Constant*> fields = llvmVals(mod, val.strct->fields);
                unsigned tsz = val.type->strct->subTypes.size();
                for (unsigned i = val.strct->fields.size(); i < tsz; ++i)
                    fields.push_back(UndefValue::get(
                        llvmType(*val.type->strct->subTypes[i])));
                 return ConstantStruct::get(fields, val.type->strct->packed);
            }
            case T_PROC: return getModuleProc(mod, val.proc->name);
            default: throw logic_error("invalid val type");
        }
    }

    static llvm::Value* compileConst(ProcEnv& env, const ConstExpr& c) {
        return llvmVal(env.mod, c.val);
    }

    static CompiledValue compileVar(ProcEnv& env, const VarExpr& v) {
        try {
            return env.bindings.get(v.name);
        } catch (const EnvKeyError&) {
            throw runtime_error("undefined variable: " + v.name);
        }
//         NameMap::const_iterator itr = env.bindings.find(v.name);
//         if (itr != env.bindings.end()) return itr->second;
//         else {
//             itr = env.globals->find(v.name);
//             if (itr != env.globals->end()) return itr->second;
//         }
//         throw runtime_error("undefined variable: " + v.name);
    }

    static llvm::Value* compileAssign(ProcEnv& env, const AssignExpr& a) {
        try {
            env.bindings.add(a.lhs, compileExpr(env, *a.rhs));
        } catch (const EnvKeyError&) {
            throw runtime_error("multiple assignment: " + a.lhs);
        }
//         NameMap::const_iterator itr = env.bindings.find(a.lhs);
//         if (itr != env.bindings.end())
//             throw runtime_error("multiple assignment: " + a.lhs);
//         env.bindings[a.lhs] = compileExpr(env, *a.rhs);
        return (llvm::Value*)0;
    }

    static void compileAlt(ProcEnv& env,
                           const ExprList& alt,
                           BasicBlock* altBlock,
                           BasicBlock* contBlock,
                           PHINode* phi) {
        env.builder.SetInsertPoint(altBlock);
        if (!alt.empty()) {
            ExprList::const_iterator itr = alt.begin(), end = alt.end();
            Value* altResult = 0;
            for (; itr != end; ++itr)
                altResult = compileExpr(env, **itr).second;
            env.builder.CreateBr(contBlock);
            altBlock = env.builder.GetInsertBlock();
            if (phi != 0 && altResult != 0) { // allow alts to simply return
//                 if (altResult == 0)
//                     throw runtime_error("case alt missing necessary result");
                phi->addIncoming(altResult, altBlock);
            }
        } else env.builder.CreateBr(contBlock);
        //else env.builder.CreateUnreachable();
    }

    static llvm::Value* compileCase(ProcEnv& env, const CaseExpr& c) {
        Function* parent = env.builder.GetInsertBlock()->getParent();
        BasicBlock* contBlock = BasicBlock::Create("case.cont", parent);
        BasicBlock* defBlock = BasicBlock::Create("case.default", parent,
                                                  contBlock);
        Value* scrut = compileExpr(env, *c.scrutinee).second;
        SwitchInst* sw = env.builder.CreateSwitch(scrut, defBlock,
                                                  c.alts.size());
        env.builder.SetInsertPoint(contBlock);
        PHINode* phi = 0;
        if (c.contType->type != T_VOID)
            phi = env.builder.CreatePHI(llvmType(*c.contType), "case.result");
        compileAlt(env, c.defAlt, defBlock, contBlock, phi);
        AltList::const_iterator itr = c.alts.begin(), end = c.alts.end();
        for (; itr != end; ++itr) {
            BasicBlock* altBlock = BasicBlock::Create("case.alt", parent,
                                                      defBlock);
            sw->addCase(llvmConstInt(scrut->getType(), itr->first), altBlock);
            compileAlt(env, itr->second, altBlock, contBlock, phi);
        }
        env.builder.SetInsertPoint(contBlock);
        return phi;
    }

    static llvm::Value* compileCast(ProcEnv& env, const CastExpr& c) {
        CompiledValue v = compileExpr(env, *c.val);
        const llvm::Type* target = llvmType(*c.target);
        Value* res;
        switch (c.type) { // begin switch of doom
            case CAST_RESIZE:
                assert (c.target->type == v.first->type);
                switch (v.first->type) {
                    case T_INT:
                        if (c.target->bits > v.first->bits)
                            res = env.builder.CreateSExt(v.second, target);
                        else res = env.builder.CreateTrunc(v.second, target);
                        break;
                    case T_UINT:
                        if (c.target->bits > v.first->bits)
                            res = env.builder.CreateZExt(v.second, target);
                        else res = env.builder.CreateTrunc(v.second, target);
                        break;
                    case T_FLOAT:
                        if (c.target->bits > v.first->bits)
                            res = env.builder.CreateFPExt(v.second, target);
                        else res = env.builder.CreateFPTrunc(v.second, target);
                        break;
                    default: throw logic_error("invalid resize type");
                } break;
            case CAST_CONVERT:
                switch (v.first->type) {
                    case T_PTR:
                        switch (c.target->type) {
                            case T_INT:
                            case T_UINT:
                                res = env.builder.CreatePtrToInt(v.second,
                                                                 target);
                                break;
                            default: throw logic_error("invalid ptr convert");
                        } break;
                    case T_INT:
                        switch (c.target->type) {
                            case T_PTR:
                                res = env.builder.CreateIntToPtr(v.second,
                                                                 target);
                                break;
                            case T_UINT: res = v.second; break;
                            case T_FLOAT:
                                res = env.builder.CreateSIToFP(v.second,
                                                               target);
                                break;
                            default: throw logic_error("invalid int convert");
                        } break;
                    case T_UINT:
                        switch (c.target->type) {
                            case T_PTR:
                                res = env.builder.CreateIntToPtr(v.second,
                                                                 target);
                                break;
                            case T_INT: res = v.second; break;
                            case T_FLOAT:
                                res = env.builder.CreateUIToFP(v.second,
                                                               target);
                                break;
                            default: throw logic_error("invalid uint convert");
                        } break;
                    case T_FLOAT:
                        switch (c.target->type) {
                            case T_INT:
                                res = env.builder.CreateFPToSI(v.second,
                                                               target);
                                break;
                            case T_UINT:
                                res = env.builder.CreateFPToUI(v.second,
                                                               target);
                                break;
                            default: throw logic_error("invalid float convert");
                        } break;
                    default: throw logic_error("invalid convert type");
                } break;
            case CAST_REINTERP: // bitcast
                switch (v.first->type) {
                    case T_PTR: case T_INT: case T_UINT: case T_FLOAT:
                        res = env.builder.CreateBitCast(v.second, target);
                        break;
                    default:
//                     case T_VOID: case T_PTR: T_ARRAY: case T_STRUCT:
//                     case T_PROC: case T_OPAQUE:
                        // do these make sense? probably not
                        throw logic_error("invalid reinterp type");
                } break;
        }
        return res;
    }

    static llvm::Value* compileUnOp(ProcEnv& env, const OpExpr& op,
                                     const sir::Type* argType, Value* arg) {
        switch (op.type) {
            case OP_NEG: return env.builder.CreateNeg(arg);
            case OP_NOT: return env.builder.CreateNot(arg);
                //            case OP_ISNAN: return;
            case OP_LOAD: return env.builder.CreateLoad(arg);
            default: throw logic_error("invalid unary op");
        }
    }

#define ARITH_OP(llvmName)\
    return env.builder.Create##llvmName(arg1, arg2);\

#define DIVIS_OP(llvmName)\
    switch (arg1type->type) {\
        case T_INT: return env.builder.CreateS##llvmName(arg1, arg2);\
        case T_UINT: return env.builder.CreateU##llvmName(arg1, arg2);\
        case T_FLOAT: return env.builder.CreateF##llvmName(arg1, arg2);\
        default: throw runtime_error("non-divisible type");\
    }

#define BITWISE_OP(llvmName)\
    switch (arg1type->type) {\
        case T_INT:\
        case T_UINT: return env.builder.Create##llvmName(arg1, arg2);\
        default: throw runtime_error("non-bitwise type");\
    }

#define CMP_OP(llvmName)\
    switch (arg1type->type) {\
        case T_INT:\
        case T_UINT: return env.builder.CreateICmp##llvmName(arg1, arg2);\
        case T_FLOAT: return env.builder.CreateFCmpO##llvmName(arg1, arg2);\
        default: throw runtime_error("non-comparable type");\
    }

#define CMPT_OP(llvmName)\
    switch (arg1type->type) {\
        case T_INT: return env.builder.CreateICmpS##llvmName(arg1, arg2);\
        case T_UINT: return env.builder.CreateICmpU##llvmName(arg1, arg2);\
        case T_FLOAT: return env.builder.CreateFCmpO##llvmName(arg1, arg2);\
        default: throw runtime_error("non-comparable type");\
    }

    static llvm::Value* compileBinOp(ProcEnv& env, const OpExpr& op,
                                     const sir::Type* arg1type, Value* arg1,
                                     const sir::Type* arg2type, Value* arg2) {
        switch (op.type) {
            case OP_ADD: ARITH_OP(Add)
            case OP_SUB: ARITH_OP(Sub)
            case OP_MUL: ARITH_OP(Mul)
            case OP_DIV: DIVIS_OP(Div)
            case OP_REM: DIVIS_OP(Rem)
            case OP_SHL:
                switch (arg1type->type) {
                    case T_INT:
                    case T_UINT: return env.builder.CreateShl(arg1, arg2);
                    default: throw runtime_error("non-shiftable type");
                }
            case OP_SHR:
                switch (arg1type->type) {
                    case T_INT: env.builder.CreateAShr(arg1, arg2);
                    case T_UINT: env.builder.CreateLShr(arg1, arg2);
                    default: throw runtime_error("non-shiftable type");
                }
            case OP_AND: BITWISE_OP(And)
            case OP_OR: BITWISE_OP(Or)
            case OP_XOR: BITWISE_OP(Xor)
            case OP_EQ: CMP_OP(EQ)
            case OP_NEQ: CMP_OP(NE)
            case OP_LT: CMPT_OP(LT)
            case OP_LTEQ: CMPT_OP(LE)
            case OP_GT: CMPT_OP(GT)
            case OP_GTEQ: CMPT_OP(GE)
            case OP_OFFSET: return env.builder.CreateGEP(arg1, arg2);
                //{
//                 const sir::Type* t;
//                 switch (arg1type->type) {
//                     case T_PTR: t = arg1type->subType; break;
//                         //                    case
//                     default: throw runtime_error("invalid offset type");
//                 }
                //            }
                // args flipped intentionally
            case OP_STORE: return env.builder.CreateStore(arg2, arg1);
            default: throw logic_error("invalid binary op");
        }
    }

    static llvm::Value* compileTernOp(ProcEnv& env, const OpExpr& op,
                                       const sir::Type* arg1type, Value* arg1,
                                       const sir::Type* arg2type, Value* arg2,
                                       const sir::Type* arg3type, Value* arg3) {
        //        default:
            throw logic_error("invalid ternary op");
    }

    static llvm::Value* compileAggOp(ProcEnv& env, const OpExpr& op,
                                      const sir::Type* arg1type, Value* arg1) {
        switch (op.type) {
            case OP_SELECT: {
                unsigned index = indexFromExpr(*op.args.at(1));
                if (arg1type->type != T_PTR)
                    throw runtime_error("first arg to select must be a ptr");
                return env.builder.CreateStructGEP(arg1, index);
            }
            case OP_EXTRACT: {
                unsigned index = indexFromExpr(*op.args.at(1));
                return env.builder.CreateExtractValue(arg1, index);
            }
            case OP_INSERT: {
                CompiledValue arg2 = compileExpr(env, *op.args.at(1));
                unsigned index = indexFromExpr(*op.args.at(2));
                return env.builder.CreateInsertValue(arg1, arg2.second, index);
            }
            default: throw logic_error("invalid aggregate op");
        }
    }
    
    static llvm::Value* compileOp(ProcEnv& env, const OpExpr& op) {
        if (op.type >= OP_SELECT) {//OP_NEG) {
            CompiledValue arg1 = compileExpr(env, *op.args.at(0));
            if (op.type < OP_NEG)
                return compileAggOp(env, op, arg1.first, arg1.second);
            if (op.type >= OP_ADD) {
                CompiledValue arg2 = compileExpr(env, *op.args.at(1));
                if (op.type > OP_STORE) {
                    CompiledValue arg3 = compileExpr(env, *op.args.at(2));
                    return compileTernOp(env, op, arg1.first, arg1.second,
                                         arg2.first, arg2.second,
                                         arg3.first, arg3.second);
                } else return compileBinOp(env, op, arg1.first, arg1.second,
                                           arg2.first, arg2.second);
            } else return compileUnOp(env, op, arg1.first, arg1.second);
        }
        throw logic_error("invalid op");
    }

    // todo: function value, not name
    static llvm::Value* compileCall(ProcEnv& env, const CallExpr& c) {
        //        Function* f = env.mod->getFunction(c.proc->name); // todo: check type
        //        if (f == 0) throw runtime_error("calling undeclared function: "
        //                                        + c.proc->name);
        CompiledValue proc = compileExpr(env, *c.proc);
        vector<Value*> args;
        ArgList::const_iterator itr = c.args.begin(), end = c.args.end();
        for (; itr != end; ++itr)
            args.push_back(compileExpr(env, **itr).second); // check type
        return env.builder.CreateCall(proc.second, args.begin(), args.end());
    }

    static llvm::Value* compileReturn(ProcEnv& env, const ReturnExpr& r) {
        if (r.val != 0) {
            CompiledValue ret = compileExpr(env, *r.val);
            if (ret.first == 0 || ret.first->type == T_VOID)
                return env.builder.CreateRetVoid();
            return env.builder.CreateRet(ret.second);
        } else return env.builder.CreateRetVoid();
    }

    static llvm::Value* compileTypeAttr(ProcEnv& env, const TypeAttrExpr& e) {
        switch (e.type) {
            case TYPEATTR_SIZEOF: {
                Type pt = {T_PTR};
                pt.subType = e.tval;
                Value* gep = env.builder.CreateGEP(
                  ConstantPointerNull::get(PointerType::get(llvmType(*e.tval),
                                                            0)),
                  ConstantInt::get(llvmType(intType), 1, false));
                return env.builder.CreatePtrToInt(gep, llvmType(uintType));
            }
            default: throw logic_error("invalid typeattr");
        }
    }

    static CompiledValue compileExpr(ProcEnv& env, const Expr& e) {
        llvm::Value* v;
        switch (e.type) {
            case EXPR_Const: v = compileConst(env, *e.cnst); break;
            case EXPR_Var: return compileVar(env, *e.var);
            case EXPR_Assign: v = compileAssign(env, *e.assign); break;
            case EXPR_Case: v = compileCase(env, *e.cse); break;
            case EXPR_Cast: v = compileCast(env, *e.cast); break;
            case EXPR_Op: v = compileOp(env, *e.op); break;
            case EXPR_Call: v = compileCall(env, *e.call); break;
            case EXPR_Return: v = compileReturn(env, *e.ret); break;
            case EXPR_TypeAttr: v = compileTypeAttr(env, *e.tattr); break;
            default: throw logic_error("invalid expr type");
        }
        return make_pair(exprType<llvm::Value*>(env.bindings, e), v);
    }

    static GlobalValue::LinkageTypes llvmLink(LinkType link) {
        switch (link) {
            case LINK_External: return GlobalValue::ExternalLinkage;
            case LINK_Internal: return GlobalValue::InternalLinkage;
            default: throw logic_error("invalid linkage type");
        }
    }

    void setGlobal(LLVMCompilerEnv& cenv,
                   const Name& name, const CompiledValue& val) {
        try {
            cenv.globals.add(name, val);
        } catch (const EnvKeyError&) {
            throw runtime_error("multiple global definitions for " + name);
        }
    }

    Function* compileProcSig(LLVMCompilerEnv& cenv, const ProcSig& s) {
        if (s.type->type != T_PROC) throw logic_error("invalid proc type");
        FunctionType* ft =
            FunctionType::get(llvmType(*s.type->proc->retType),
                              llvmTypes(s.type->proc->paramTypes), false);
        Function* f = cenv.mod->getFunction(s.name);
        if (f == 0) {
            f = Function::Create(ft, llvmLink(s.link), s.name, cenv.mod);
            if (s.name != "") setGlobal(cenv, s.name, make_pair(s.type, f));
        }
        else if (f->getFunctionType() != ft)
            throw runtime_error("invalid redeclaration of proc: " + s.name);
        return f;
    }

    Function* compileProcDef(LLVMCompilerEnv& cenv, const ProcDef& d) {
        ProcEnv env(cenv);
//         env.mod = cenv.mod;
//         env.globals = &cenv.globals;
        Function* f = compileProcSig(cenv, *(d.sig));
        if (!f->empty()) throw runtime_error("invalid redefinition of proc: "
                                             + d.sig->name);
        if (d.paramNames.size() != d.sig->type->proc->paramTypes.size())
            throw runtime_error("proc params mismatch: " + d.sig->name);
        {
            Function::arg_iterator itr = f->arg_begin(), end = f->arg_end();
            NameList::const_iterator nameItr = d.paramNames.begin();
            TypeList::const_iterator typeItr = d.sig->type->proc->paramTypes.begin();
            for (; itr != end; ++itr, ++nameItr, ++typeItr) {
                itr->setName(*nameItr);
                env.bindings.add(*nameItr, make_pair(*typeItr, itr));
            }
        }
        BasicBlock* bb = BasicBlock::Create("entry", f);
        env.builder.SetInsertPoint(bb);
        ExprList::const_iterator itr = d.body.begin(), end = d.body.end();
        for (; itr != end; ++itr)
            compileExpr(env, **itr);
        verifyFunction(*f);
        if (cenv.fpm != 0) cenv.fpm->run(*f);
        return f;
    }

    static GlobalVariable* llvmGlobal(LLVMCompilerEnv& cenv,
                                      const GlobalSig& gs, Constant* val) {
        if (gs.type->type != T_PTR) throw logic_error("global must be ptr");
        GlobalVariable* g = new GlobalVariable(llvmType(*gs.type->subType),
                                               gs.constant, llvmLink(gs.link),
                                               val, gs.name,
                                               cenv.mod, gs.threadLocal);
        setGlobal(cenv, gs.name, make_pair(gs.type, g)); 
        return g;
    }

    GlobalVariable* compileGlobalSig(LLVMCompilerEnv& cenv,
                                     const GlobalSig& gs) {
        return llvmGlobal(cenv, gs, 0);
    }

    GlobalVariable* compileGlobalDef(LLVMCompilerEnv& cenv,
                                     const GlobalDef& g) {
        return llvmGlobal(cenv, *g.sig, llvmVal(cenv.mod, g.val));
    }
}
*/
