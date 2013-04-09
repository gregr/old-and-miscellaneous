#include "sir_llvm.h"
#include "sirc_parse.h"
#include <iostream>
#include <fstream>
using namespace sir;
//using namespace llvm;
using namespace std;
//using std::cin;
//using std::cout;

// hopefully one day this will be callable from the JIT...
extern "C"
void hello(int v) { printf("test: %d\n", v); }

void prompt() { cout << "\nsir> " << flush; }

void interpret(LLVMExecEnv& ee, ParseEnv& penv, istream& in,
               bool showPrompt=true, bool dumping=true) {
    bool parsing = true;
    while (parsing) {
        if (showPrompt) prompt();
        TopVal top = parseTop(penv, in);
        llvm::Function* f;
        switch (top.type) {
            case TOP_TYPE: //cout << "parsed type" << endl;
                break;
            case TOP_SIG: //cout << "parsed sig" << endl;
                f = execProcSig(ee, *top.sig);
                if (dumping) f->dump();
                break;
            case TOP_DEF: //cout << "parsed def" << endl;
                f = execProcDef(ee, *top.def);//, false);
                if (dumping) f->dump();
                break;
            case TOP_EXPR: //cout << "parsed expr" << endl;
                cout << showVal(execExpr(ee, *top.expr)) << endl;
                break;
            case TOP_EOF: parsing = false; break;
        }
    }
    //    if (showPrompt) cout << "\ndone" << endl;
}

// void compile(LLVMExecEnv& ee, ParseEnv& penv, istream& in) {
//     bool parsing = true;
//     while (parsing) {
//         TopVal top = parseTop(penv, cin);
//         llvm::Function* f;
//         switch (top.type) {
//             case TOP_TYPE: break;
//             case TOP_SIG:
//                 f = execProcSig(ee, *top.sig);
//                 if (dumping) f->dump();
//                 break;
//             case TOP_DEF:
//                 f = execProcDef(ee, *top.def);
//                 if (dumping) f->dump();
//                 break;
//             case TOP_EXPR:
//                 //                cout << showVal(execExpr(ee, *top.expr)) << endl;
//                 break;
//             case TOP_EOF: parsing = false; break;
//         }
//     }
// }

// void compileFile(LLVMExecEnv& ee, ParseEnv& penv, const char* fileName) {
//     compile(ee, penv, ifstream(fileName));
// }

int main(int argc, const char** argv) {
    try {
        LLVMExecEnv ee;
        initExecEnv(ee);
        ParseEnv penv;
        initParseEnv(penv);
        bool dumping = true;
        if (argc == 1) interpret(ee, penv, cin, true, dumping);
        else {
            ifstream fin(argv[1]);
            interpret(ee, penv, fin, false, dumping);
            interpret(ee, penv, cin, true, dumping);
        }
//         else {
//             for (int i = argc; i > 1; --i)
//                 compileFile(ee, penv, argv[i]);
//         }
        if (dumping) ee.cenv.mod->dump();
    } catch (const exception& e) {
        cout << e.what() << endl;
    } catch (...) {
        cout << "unknown exception" << endl;
    }
    return 0;
}
