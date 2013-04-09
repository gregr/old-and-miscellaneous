#include "sir_llvm.h"
#include "sir_parse.h"
#include <llvm/Analysis/Verifier.h>
#include <llvm/Support/raw_ostream.h>
//#include <iostream>
using namespace sir;
using namespace llvm;
using namespace std;

void test();

int main() {
    try {
        test();
    } catch (const exception& e) {
        cout << e.what() << endl;
    } catch (...) {
        cout << "unknown exception" << endl;
    }
    return 0;
}

void test() {
    sir::Type intType = {T_INT};
    intType.bits = 32;
    sir::Type floatType = {T_FLOAT};
    floatType.bits = 32;

    ProcType mainType;
    mainType.retType = &intType;
    mainType.paramTypes.push_back(&intType);
    ProcSig sig = {{"testMain"}, &mainType};
    ProcDef def;
    def.sig = &sig;
    def.paramNames.push_back("arg");

    ProcType fibType;
    fibType.retType = &intType;
    fibType.paramTypes.push_back(&intType);
    fibType.paramTypes.push_back(&intType);
    fibType.paramTypes.push_back(&intType);
    ProcSig fibSig = {{"fib"}, &fibType};
    ProcDef fibDef;
    fibDef.sig = &fibSig;
    fibDef.paramNames.push_back("count");
    fibDef.paramNames.push_back("a");
    fibDef.paramNames.push_back("b");

    ProcType simpProcType;
    simpProcType.retType = &floatType;
    simpProcType.paramTypes.push_back(&intType);
    ProcSig simpSig = {{"simp"}, &simpProcType};
    ProcDef simpDef;
    simpDef.sig = &simpSig;
    simpDef.paramNames.push_back("arg");

    //        ConstExpr zero = {{&intType, {0}}}; // this works
    ConstExpr zero;
    zero.val.type = &intType;
    zero.val.i = 0;
    Expr zeroE = {EXPR_Const, {&zero}};
    ConstExpr one;
    one.val.type = &intType;
    one.val.i = 1;
    //        ConstExpr one = {{&intType, {1}}}; // this fails... damn you C
    Expr oneE;
    oneE.type = EXPR_Const;
    oneE.cnst = &one;

    VarExpr v1 = {"arg"};
    Expr v1E;
    v1E.type = EXPR_Var;
    v1E.var = &v1;

    CallExpr fibCall;
    fibCall.proc = &fibSig;
    fibCall.args.push_back(&v1E);
    fibCall.args.push_back(&zeroE);
    fibCall.args.push_back(&oneE);
    Expr fibCallE;
    fibCallE.type = EXPR_Call;
    fibCallE.call = &fibCall;

    ReturnExpr ret = {&fibCallE};
    Expr retE;
    retE.type = EXPR_Return;
    retE.ret = &ret;

    VarExpr vcount = {"count"};
    Expr vcountE;
    vcountE.type = EXPR_Var;
    vcountE.var = &vcount;
    VarExpr va = {"a"};
    Expr vaE;
    vaE.type = EXPR_Var;
    vaE.var = &va;
    VarExpr vb = {"b"};
    Expr vbE;
    vbE.type = EXPR_Var;
    vbE.var = &vb;

    OpExpr cmp;
    cmp.type = OP_LTEQ;
    cmp.args.push_back(&vcountE);
    cmp.args.push_back(&zeroE);
    Expr cmpE;
    cmpE.type = EXPR_Op;
    cmpE.op = &cmp;

    OpExpr sub;
    sub.type = OP_SUB;
    sub.args.push_back(&vcountE);
    sub.args.push_back(&oneE);
    Expr subE;
    subE.type = EXPR_Op;
    subE.op = &sub;

    OpExpr add;
    add.type = OP_ADD;
    add.args.push_back(&vaE);
    add.args.push_back(&vbE);
    Expr addE;
    addE.type = EXPR_Op;
    addE.op = &add;

    CallExpr call;
    call.proc = &fibSig;
    call.args.push_back(&subE);
    call.args.push_back(&vbE);
    call.args.push_back(&addE);
    Expr callE;
    callE.type = EXPR_Call;
    callE.call = &call;

    CaseExpr cse;
    cse.scrutinee = &cmpE;
    cse.contType = &intType;
    //        cse.defAlt.push_back(&mul);
    cse.alts.resize(2);
    cse.alts[0].first = 0;
    cse.alts[0].second.push_back(&callE);
    cse.alts[1].first = 1;
    cse.alts[1].second.push_back(&vaE);
    //        cse.alts.push_back(make_pair(, ExprList()));
    Expr cseE;
    cseE.type = EXPR_Case;
    cseE.cse = &cse;

    ReturnExpr fibRet;
    fibRet.val = &cseE;
    //        fibRet.val = &callE;
    Expr fibRetE;
    fibRetE.type = EXPR_Return;
    fibRetE.ret = &fibRet;

    fibDef.body.push_back(&fibRetE);
    def.body.push_back(&retE);

    ConstExpr cf;
    cf.val.type = &floatType;
    cf.val.f = 1.4;
    Expr cfE = {EXPR_Const, {&cf}};

    CastExpr cst = {CAST_CONVERT, &floatType, &v1E};
    Expr cstE = {EXPR_Cast};
    cstE.cast = &cst;

    OpExpr simpAdd;
    simpAdd.type = OP_ADD;
    simpAdd.args.push_back(&cstE);
    simpAdd.args.push_back(&cfE);
    Expr simpAddE;
    simpAddE.type = EXPR_Op;
    simpAddE.op = &simpAdd;

    ReturnExpr simpRet;
    simpRet.val = &simpAddE;
    Expr simpRetE;
    simpRetE.type = EXPR_Return;
    simpRetE.ret = &simpRet;

    simpDef.body.push_back(&simpRetE);

    LLVMExecEnv exec;
    initExecEnv(exec);
    execProcDef(exec, simpDef);
    execProcDef(exec, fibDef);
    execProcDef(exec, def);
    verifyModule(*exec.mod, PrintMessageAction);
    exec.pm.run(*exec.mod);
    exec.mod->dump();
    vector<GenericValue> args(1);
    args[0].IntVal = APInt(32, 8);
    Val r1 = execCall(exec, simpSig, args);
    Val r2 = execCall(exec, sig, args);
    Val r3 = execExpr(exec, oneE);
    Val r4 = execExpr(exec, zeroE);
    outs() << "simp result: " << showVal(r1) << "\n";
    outs() << "fib result: " << showVal(r2) << "\n";
    outs() << "expr result: " << showVal(r3) << "\n";
    outs() << "expr result: " << showVal(r4) << "\n";
    outs().flush();
}
