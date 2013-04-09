#ifndef prim_llvm_H_
#define prim_llvm_H_

// #include <llvm/DerivedTypes.h>
#include <llvm/Module.h>

#include <string>

enum PrimType { T_ADDR, T_TAG, T_CHAR, T_INT, T_UINT, T_FLOAT, T_DOUBLE };

enum BinOp { OP_ADD, OP_MUL, OP_LT };

typedef std::string Name;

typedef int Tag;

struct Env;

// todo: visitor pattern for analysis instead to separate prim from llvm
struct Expr : public Expr {
    virtual ~Expr() {}
    virtual void eval(Env&) const = 0;
};

template <typename ConstType>
struct ConstExpr : public Expr {
    IntConst(ConstType val_) : val(val_) {}
    void eval(Env&) const;
    ConstType val;
};

struct VarExpr : public Expr {
    VarExpr(const Name& name_) : name(name_) {}
    void eval(Env&) const;
    Name name;
};

struct AllocExpr : public Expr {
    AllocExpr(const Expr& size_) : size(size_) {}
    void eval(Env&) const;
    const Expr& size;
};

//enum MemOp { MEM_STORE, MEM_LOAD };

struct MemOpExpr : public Expr {
    MemOpExpr(const Expr& addr_, const Expr& offset_, const Expr* storeVal_=0)
        : addr(addr_), offset(offset_), storeVal(storeVal_) {}
    void eval(Env&) const;
    //    MemOp op_;
    const Expr& addr;
    const Expr& offset;
    const Expr& storeVal;
};

typedef std::vector<std::pair<ConstantInt*, Expr*> > AltList;

struct CaseExpr : public Expr {
    CaseExpr(const Expr& val_, const Expr& defAlt_, const AltList& alts_,
             const Type* contType_)
        : val(val_), defAlt(defAlt_), alts(alts_), contType(contType_) {}
    void eval(Env&) const;
    AltList alts;
    const Expr &val, &defAlt;
    const Type* contType;
};

struct BinOpExpr : public Expr {
    BinOpExpr(BinOp op_, const Expr& lhs_, const Expr& rhs_)
        : op(op_), lhs(lhs_), rhs(rhs_) {}
    void eval(Env&) const;
    BinOp op;
    const Expr &lhs, &rhs;
};

typedef std::vector<Expr*> ArgList;

struct CallExpr : public Expr {
    CallExpr(const Name& procName_, const ArgList& args_)
        : procName(procName_), args(args_) {}
    void eval(Env&) const;
    ArgList args;
    Name procName;
};

typedef std::vector<const Type*> TypeList;
typedef std::vector<Name> NameList;

struct ProcSig {
    ProcSig(const Name& name_, const Type* retType_,
            const TypeList& paramTypes_)
        : name(name_), retType(retType_), paramTypes(paramTypes_) {}
    llvm::Function* analyze(llvm::Module&) const;
    size_t numParams() const { return paramTypes_.size(); }
    TypeList paramTypes;
    Name name;
    const Type* retType;
};

struct ProcDef {
    ProcDef(const Name& name_, const Type* retType_,
            const TypeList& paramTypes_,
            const NameList& paramNames_, const Expr& body_)
        : sig(retType_, paramTypes_), paramNames(paramNames_), body(body_) {}
    llvm::Function* analyze(llvm::Module&) const;
    ProcSig sig;
    NameList paramNames;
    const Expr& body;
};

#endif
