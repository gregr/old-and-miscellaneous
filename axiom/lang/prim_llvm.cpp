#include "prim_llvm.h"
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Support/IRBuilder.h>
#include <map>
using namespace llvm;
using namespace std;

typedef map<Name, Value*> NameMap;

struct LLVMEnv {
    LLVMEnv(Module& m) : mod(m) {}
    Module& mod;
    IRBuilder<> builder;
    NameMap bindings;
    Value* result;
};

Module* makeModule() {
    Module* mod;
    verifyModule(*mod, PrintMessageAction);
    PassManager pm;
    PrintModulePass printPass(&llvm::cout);
    pm.add(&printPass);
    pm.run(*mod);
    return mod;
}

std::vector<GenericValue> GenList;

void execute() {
    ExistingModuleProvider mp = ExistingModuleProvider(m);
    ExecutionEngine* ee = ExecutionEngine::create(&mp, false);
    GenList args(1);
    args[0].IntVal = APInt(32, n);
    GenericValue gv = ee->runFunction(proc, args);
    gv.IntVal;
}

struct Env {
    virtual ~Env() {}
    virtual void evalVar(const VarExpr& e) = 0;
};

struct LLVMEnv : public Env {
    void evalVar(const VarExpr& e);
};

void VarExpr::eval(Env& env) const {
    NameMap::const_iterator itr = env.bindings.find(name_);
    if (itr == env.bindings.end())
        ; // todo: error
    return *itr;
}

void ConstExpr::eval(Env& env) const {
    return analyzeConst(val_);
}

typedef Type::Int32Ty WordType;

void AllocExpr::eval(Env& env) const {
    //    const IntegerType* wordType = IntegerType::get(sizeof(Word)*8);
    // todo: use runtime heap alloc
    return env.builder.CreateMalloc(WordType, size_.eval(env));
}

void MemOpExpr::eval(Env& env) const {
    Value* p = env.builder.CreateGEP(addr_.eval(env), offset_.eval(env));
    if (storeVal_ != 0)
        return env.builder.CreateStore(storeVal_->eval(env), p);
    else
        return env.builder.CreateLoad(p);
}

void CaseExpr::eval(Env& env) const {
    Function* parent = env.builder.GetInsertBlock()->getParent();
    BasicBlock* defBlock = BasicBlock::Create("default");
    SwitchInst* sw = env.builder.CreateSwitch(val_.eval(env), defBlock,
                                              alts_.size());
    BasicBlock* contBlock = BasicBlock::Create("cont", parent);
    env.builder.setInsertPoint(contBlock);
    PHINode* phi = env.builder.CreatePHI(contType_, "caseResult");
    evalAlt(env, parent, defAlt_, defBlock, contBlock, phi);
    AltList::const_iterator itr = alts_.begin(), end = alts_.end();
    for (; itr != end; ++itr) {
        BasicBlock* altBlock = BasicBlock::Create("alt");
        sw->addCase(itr->first, altBlock);
        evalAlt(env, parent, itr-second, altBlock, contBlock, phi);
    }
    parent->getBasicBlockList().push_back(contBlock);
    env.builder.setInsertPoint(contBlock);
    return phi;
}

void evalAlt(Env& env, Function* parent, const Expr& alt,
                BasicBlock* altBlock, BasicBlock* contBlock, PHINode* phi) {
    env.builder.SetInsertPoint(altBlock);
    Value* altResult = alt->eval(env);
    env.builder.CreateBr(contBlock);
    altBlock = env.builder.GetInsertBlock();
    parent->getBasicBlockList().push_back(altBlock);
    phi->addIncoming(altResult, altBlock);
}

void BinOpExpr::eval(Env& env) const {
    Value* l = lhs_.eval(env);
    Value* r = rhs_.eval(env);
    switch (op_) {
        case OP_ADD: return env.builder.CreateAdd(l, r);
        case OP_MUL: return env.builder.CreateMul(l, r);
        case OP_LT:
            l = env.builder.Create(l, r);
    }
}

void CallExpr::eval(Env& env) const {
    return env.builder.CreateCall();
}

Function* ProcSig::eval(Module& mod) const {
    FunctionType* ft = FunctionType::get(retType_, paramTypes_, false);
    Function* f = mod.getFunction(name_, ft);
    if (f == 0)
        f = Function::Create(ft, Function::ExternalLinkage, name_, &mod);
    return f;
}

Function* ProcDef::eval(Module& mod) const {
    Function* f = sig_.eval(mod);
    if (!f->empty())
        ; // todo: redef error
    Env env(mod);
    if (paramNames_.size() != sig_.numParams())
        ; // todo: error
    Function::arg_iterator itr = f->arg_begin(), end = f->arg_end();
    NameList::const_iterator nameItr = paramNames_.begin();
    for (; itr != end; ++itr, ++nameItr) {
        itr->setName(*nameItr);
        env.bindings[*nameItr] = itr;
    }
    BasicBlock* bb = BasicBlock::Create("entry", f);
    env.builder.SetInsertPoint(bb);
    env.builder.CreateRet(body_.eval(env));
    verifyFunction(f);
    return proc;
}

void analyzeConst(int v) const {
    // todo:
    //    return ConstantFP::get(APFloat(v));
}

void analyzeConst(int v) const {
    return ConstantFP::get(APInt(v));
}

void analyzeConst(double v) const {
    return ConstantFP::get(APFloat(v));
}
