#ifndef axiom_lang_sirc_parse_H_
#define axiom_lang_sirc_parse_H_

#include "sir.h"
#include <istream>
#include <map>

namespace sir {

    enum TokenType { // todo: unlink/hide?
        TOK_LPAREN, TOK_RPAREN, TOK_LBRACE, TOK_RBRACE,
        TOK_LSQUARE, TOK_RSQUARE, TOK_LANGLE, TOK_RANGLE,
        TOK_SEPARATOR, TOK_ENDSTMT, TOK_ASSIGN,
        TOK_TYPE, TOK_TYPEVAL, TOK_VAL, TOK_IDENT,
        TOK_GLOBAL, TOK_CONST, TOK_PTR, TOK_PACKED,
        TOK_SIG, TOK_DEF, TOK_CASE,
        TOK_CAST, TOK_OP, TOK_CALL, TOK_RET, TOK_TYPEATTR,
        TOK_COMMAND, TOK_EOF
    };

    struct Token {
        TokenType type;
        Name ident;
        union {
            Val val;
            const Type* t;
            OpType op;
            CastType cast;
            TypeAttrType tattr;
        };
    };

    Token getToken(std::istream&);

    typedef std::map<Name, const ProcSig*> SigMap;
    typedef std::map<Name, const ProcDef*> DefMap;
    typedef std::map<Name, const Type*> TypeMap;

    struct ParseEnv {
        SigMap sigs;
        DefMap defs;
        TypeMap types;
        std::vector<ProcSig*> allocSig;
        std::vector<ProcDef*> allocDef;
        std::vector<ArrayVal*> allocArrayVal;
        std::vector<StructVal*> allocStructVal;
        std::vector<Type*> allocType;
        std::vector<ArrayType*> allocArrayType;
        std::vector<StructType*> allocStructType;
        std::vector<Expr*> allocExpr;
        std::vector<ConstExpr*> allocConstExpr;
        std::vector<VarExpr*> allocVarExpr;
        std::vector<AssignExpr*> allocAssignExpr;
        std::vector<CaseExpr*> allocCaseExpr;
        std::vector<CastExpr*> allocCastExpr;
        std::vector<OpExpr*> allocOpExpr;
        std::vector<CallExpr*> allocCallExpr;
        std::vector<ReturnExpr*> allocReturnExpr;
        std::vector<TypeAttrExpr*> allocTypeAttrExpr;
        std::vector<ProcType*> allocProcType;
    };

    void initParseEnv(ParseEnv&);
    void freeParseEnv(ParseEnv&);

    enum TopValType { TOP_TYPE, TOP_SIG, TOP_DEF, TOP_EXPR, TOP_EOF };

    struct TopVal {
        TopValType type;
        union {
            const Type* t;
            const ProcSig* sig;
            const ProcDef* def;
            const Expr* expr;
        };
    };

    TopVal parseTop(ParseEnv&, std::istream&);
    const Expr* parseExpr(ParseEnv&, std::istream&);
}

#endif
