#ifndef axiom_sir_llvm_impl_H_
#define axiom_sir_llvm_impl_H_

extern "C" {
#include "sir_llvm.h"
}

#include <llvm/ModuleProvider.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/PassManager.h>
#include <llvm/Module.h>

struct sir_llvm_Global {
    llvm::GlobalVariable* g;
};

struct sir_llvm_Proc {
    llvm::Function* p;
};

struct sir_llvm_Mod {
    llvm::Module* m;
};

struct sir_llvm_CompiledMod {
    Env bindings;
    sir_llvm_Mod mod;
};

struct sir_llvm_ExecState {
    sir_llvm_CompiledMod cmod;
    llvm::ExecutionEngine* ee;
};

struct sir_llvm_Passes {
};

/*
// most of the following is obsolete, but useful for reference
namespace sir {

    //    typedef std::pair<const sir::Type*, llvm::Value*> CompiledValue;
    //    typedef std::map<Name, CompiledValue> NameMap;
    typedef TypedEnv<llvm::Value*> LLVMTypedEnv;
    typedef LLVMTypedEnv::T Bindings;
    typedef LLVMTypedEnv::V CompiledValue;

    struct LLVMCompilerEnv {
        Bindings globals; // includes procs
        llvm::Module* mod;
        llvm::FunctionPassManager* fpm;
    };

    llvm::Function* compileProcSig(LLVMCompilerEnv&, const ProcSig&);
    llvm::Function* compileProcDef(LLVMCompilerEnv&, const ProcDef&);
    llvm::GlobalVariable* compileGlobalSig(LLVMCompilerEnv&, const GlobalSig&);
    llvm::GlobalVariable* compileGlobalDef(LLVMCompilerEnv&, const GlobalDef&);

    struct LLVMExecEnv {
        //        std::map<Name, Val> bindings;
        LLVMCompilerEnv cenv;
        llvm::PassManager pm;
        llvm::ExecutionEngine* ee;
        llvm::ExistingModuleProvider* mp;
        //        llvm::Module* mod;
    };

    void initExecEnv(LLVMExecEnv&);
    llvm::Function* getProcByName(LLVMExecEnv&, const Name&);
    llvm::Function* execProcSig(LLVMExecEnv&, const ProcSig&);
    llvm::Function* execProcDef(LLVMExecEnv&, const ProcDef&,
                                bool optimize=true);
    Val execCallRaw(LLVMExecEnv&, llvm::Function*, const sir::Type&,
                    const std::vector<llvm::GenericValue>&);
    Val execCall(LLVMExecEnv&, const ProcSig&,
                 const std::vector<llvm::GenericValue>&);
    Val execExpr(LLVMExecEnv&, const Expr&, bool erase=false);
}
*/
#endif
