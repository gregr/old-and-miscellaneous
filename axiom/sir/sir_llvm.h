#ifndef axiom_sir_llvm_H_
#define axiom_sir_llvm_H_

#include "sir.h"
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>

typedef struct {
    Env bindings;
    LLVMModuleRef mod;
    int ownsMod;
} sir_llvm_CompiledMod;

typedef struct {
    sir_llvm_CompiledMod cmod;
    LLVMModuleProviderRef mp;
    LLVMExecutionEngineRef ee;
} sir_llvm_ExecState;

void sir_llvm_CompiledMod_init(sir_llvm_CompiledMod*, LLVMModuleRef, int);
void sir_llvm_CompiledMod_destroy(sir_llvm_CompiledMod*);
//sir_llvm_Mod* sir_llvm_CompiledMod_mod(sir_llvm_CompiledMod*);

void sir_llvm_ExecState_init(sir_llvm_ExecState*, LLVMModuleRef, char**);
void sir_llvm_ExecState_destroy(sir_llvm_ExecState*);
//sir_llvm_CompiledMod* sir_llvm_ExecState_cmod(sir_llvm_ExecState*);
// static inline sir_llvm_Mod* sir_llvm_ExecState_mod(
//                                        sir_llvm_ExecState* es) {
//     return sir_llvm_CompiledMod_mod(sir_llvm_ExecState_cmod(es));
// }

void sir_llvm_linkMod(LLVMModuleRef, const LLVMModuleRef);
void sir_llvm_linkFile(LLVMModuleRef, const char*);

// todo: bc, llir, etc. use c++ stdiostream or stdio_filebuf with FILE
// per-function versions too?
void sir_llvm_emitBC(const LLVMModuleRef, FILE*);
void sir_llvm_emitLL(const LLVMModuleRef, FILE*);
// todo: module file loading

LLVMValueRef sir_llvm_compileProcSig(sir_llvm_CompiledMod*,
                                     const sir_ProcSig*);
LLVMValueRef sir_llvm_compileProcDef(sir_llvm_CompiledMod*,
                                     const sir_ProcDef*);
LLVMValueRef sir_llvm_compileGlobalSig(sir_llvm_CompiledMod*,
                                       const sir_GlobalSig*);
LLVMValueRef sir_llvm_compileGlobalDef(sir_llvm_CompiledMod*,
                                       const sir_GlobalDef*);

sir_Value sir_llvm_execExpr(sir_llvm_ExecState*, const sir_Expr*);

#endif
