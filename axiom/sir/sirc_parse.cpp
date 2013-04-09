#include "sirc_parse.h"
#include <sstream>
#include <map>
#include <stdexcept>
using namespace std;

namespace sir {

    typedef map<Name, OpType> OpMap;
    typedef map<Name, CastType> CastMap;
    typedef map<Name, TypeAttrType> TypeAttrMap;
    typedef map<Name, TokenType> TokenMap;
    typedef map<char, TokenType> SyntaxMap;

    static TokenMap keywords;
    static SyntaxMap syntax;
    static OpMap ops;
    static CastMap casts;
    static TypeAttrMap typeAttrs;
    //    static TypeMap types;

    static void skipSpace(istream& in) {
        while (in) {
            char ch = in.get();
            if (!isspace(ch)) {
                in.putback(ch);
                break;
            }
        }
    }

    static Token getNumeric(istream& in, bool neg) {
        Token tok = {TOK_VAL};
        string num;
        if (neg) num += '-';
        bool isFloat = false;
        while (in) {
            char ch = in.get();
            if (!isdigit(ch)) {
                if (ch == '.' && !isFloat) {
                    isFloat = true;
                    num += ch;
                } else {
                    in.putback(ch);
                    break;
                }
            } else num += ch;
        }
        if (isFloat) {
            tok.val.type = &doubleType;
            tok.val.d = atof(num.c_str());
        } else {
            tok.val.type = &intType;
            tok.val.i = atoi(num.c_str());
        }
        return tok;
    }

    static Token getNamed(istream& in) {
        Name name;
        while (in) {
            char ch = in.get();
            if (!isalnum(ch) && ch != '_') {
                in.putback(ch);
                break;
            } else name += ch;
        }{
            TokenMap::const_iterator itr = keywords.find(name);
            if (itr != keywords.end()) {
                Token tok = {itr->second};
                return tok;
            }
//         }{
//             TypeMap::const_iterator itr = types.find(name);
//             if (itr != types.end()) {
//                 Token tok = {TOK_TYPEVAL};
//                 tok.t = itr->second;
//                 return tok;
//             }
        }{
            CastMap::const_iterator itr = casts.find(name);
            if (itr != casts.end()) {
                Token tok = {TOK_CAST};
                tok.cast = itr->second;
                return tok;
            }
        }{
            OpMap::const_iterator itr = ops.find(name);
            if (itr != ops.end()) {
                Token tok = {TOK_OP};
                tok.op = itr->second;
                return tok;
            }
        }{
            TypeAttrMap::const_iterator itr = typeAttrs.find(name);
            if (itr != typeAttrs.end()) {
                Token tok = {TOK_TYPEATTR};
                tok.tattr = itr->second;
                return tok;
            }
        }
        Token tok = {TOK_IDENT};
        tok.ident = name;
        return tok;
    }

    static Token getSymbolic(char sym) {
//     static Token getSymbolic(istream& in) {
//         Name sym;
//         while (in) {
//             char ch = in.get();
//             if (isalnum(ch) || isspace(ch)) {
//                 in.putback(ch);
//                 break;
//             } else sym += ch;
//         }{
//        {
        SyntaxMap::const_iterator itr = syntax.find(sym);
        if (itr != syntax.end()) {
            Token tok = {itr->second};
            return tok;
        }
//         }{
//             OpMap::const_iterator itr = ops.find(sym);
//             if (itr != ops.end()) {
//                 Token tok = {TOK_OP};
//                 tok.op = itr->second;
//                 return tok;
//             }
//         }
        throw runtime_error("unknown symbol: " + string(1, sym));
    }

    void skipComment(istream& in) {
        while (in) {
            if (in.get() == '\n')
                break;
        }
    }

    Token getToken(istream& in) {
        for (;;) {
            skipSpace(in);
            if (!in) {
                Token tok = {TOK_EOF};
                return tok;
            }
            char ch = in.get();
            if (isalpha(ch) || ch == '_') {
                in.putback(ch);
                return getNamed(in);
            } else if (isdigit(ch)) {
                in.putback(ch);
                return getNumeric(in, false);
            } else if (ch == '-') {
//                 char next = in.get();
//                 if (isdigit(next)) {
//                     in.putback(next);
                return getNumeric(in, true);
//                 } else {
//                     in.putback(ch);
//                     return getSymbolic(in);
//                 }
            } else if (ch == '#') { // reserve hash? ## comment?
                skipComment(in);
            } else return getSymbolic(ch);
        }
    }

    static void parseSyntaxRaw(istream& in, TokenType syntax,
                               const Token& tok) {
        if (tok.type != syntax) {
            ostringstream ss;
            ss << syntax;
            string ssyn = ss.str();
            ss.str("");
            ss << tok.type;
            throw runtime_error("expected syntax: " + ssyn
                                + " but parsed: " + ss.str());
        }
    }

    static void parseSyntax(istream& in, TokenType syntax) {
        parseSyntaxRaw(in, syntax, getToken(in));
    }

//     static vector<string> parseTuple(istream& in) {
        
//     }

    static Name parseIdent(istream& in) {
        Token tok = getToken(in);
        if (tok.type != TOK_IDENT) throw runtime_error("expected ident");
        return tok.ident;
    }

    static const Type* getType(const ParseEnv& penv, const Name& typeName) {
        TypeMap::const_iterator itr = penv.types.find(typeName);
        if (itr == penv.types.end())
            throw runtime_error(typeName + " is not a type name");
        return itr->second;
    }

    template <typename ManagedType>
    ManagedType* managed(vector<ManagedType*>& manager) {
        manager.push_back(0);
        ManagedType* p = new ManagedType;
        manager.back() = p;
        return p;
    }

    static int parseInt(istream& in) {
        Token tok = getToken(in);
        if (tok.type != TOK_VAL) throw runtime_error("expected int constant");
        // todo: named constants
        if (tok.val.type->type != T_INT)
            throw runtime_error("expected int constant");
        return tok.val.i;
    }

    static const Type* parseTypeVal(ParseEnv&, istream&);

    static const Type* parseTypeValRaw(ParseEnv& penv, istream& in,
                                       const Token& tok) {
//         penv.typeAlloc.push_back(0);
//         Type* t = new Type;
//         penv.typeAlloc.back() = t;
        // todo:
        switch (tok.type) {
            case TOK_PTR: {
                Type* pt = managed(penv.allocType);
                pt->type = T_PTR;
                pt->subType = parseTypeVal(penv, in);
                return pt;
            }
            case TOK_IDENT: return getType(penv, tok.ident);
            case TOK_LSQUARE: {
                ArrayType* at = managed(penv.allocArrayType);
                at->subType = parseTypeVal(penv, in);
                at->size = parseInt(in);
                parseSyntax(in, TOK_RSQUARE);
                Type* t = managed(penv.allocType);
                t->type = T_ARRAY;
                t->arr = at;
                return t;
            }
            case TOK_LBRACE: {
                StructType* st = managed(penv.allocStructType);
                Token tok = getToken(in);
                if (tok.type == TOK_PACKED) {
                    st->packed = true;
                    tok = getToken(in);
                } else st->packed = false;
                for (;;) {
                    if (tok.type == TOK_RBRACE) break;
                    st->subTypes.push_back(parseTypeValRaw(penv, in, tok));
                    tok = getToken(in);
                }
                Type* t = managed(penv.allocType);
                t->type = T_STRUCT;
                t->strct = st;
                return t;
            }
            default: throw runtime_error("expected type value");
        }
        //        return t;
    }

    static const Type* parseTypeVal(ParseEnv& penv, istream& in) {
        return parseTypeValRaw(penv, in, getToken(in));
    }

    static const Type* parseType(ParseEnv& penv, istream& in) {
        Name ident = parseIdent(in);
        parseSyntax(in, TOK_ASSIGN);
        const Type* t = parseTypeVal(penv, in);
        parseSyntax(in, TOK_ENDSTMT);
        penv.types[ident] = t; // todo: check if it exists first
        return t;
    }

//     template <typename ExprType>
//     ExprType* allocExpr(ParseEnv& penv) {
//         penv.exprAlloc.push_back(0);
//         void* p = malloc(sizeof(ExprType));
//         penv.exprAlloc.back() = p;
//         return (ExprType*)p;
//     }

    static const Expr* buildConst(ParseEnv& penv, const Val& val) {
        Expr* e = managed(penv.allocExpr);
        ConstExpr* c = managed(penv.allocConstExpr);
        c->val = val;
        e->type = EXPR_Const;
        e->cnst = c;
        return e;
    }

//     static const Expr* buildNamedConst(ParseEnv& penv, const Name& name) {
//         throw; // todo
//     }

    static const Expr* buildVar(ParseEnv& penv, const Name& name) {
        Expr* e = managed(penv.allocExpr);
        VarExpr* v = managed(penv.allocVarExpr);//VarExpr;//managed(penv.allocVarExpr);
        v->name = name;
        e->type = EXPR_Var;
        e->var = v;
        return e;
    }

    static Val refineType(const Val& val, const Type& t) {
        //        if (val.type->type != t.type) {
            Val v;
            switch (val.type->type) {
                case T_INT:
                    switch (t.type) {
                        case T_PTR:
                            if (val.i != 0)
                                throw runtime_error("ptr literal must be 0");
                            break;
                        case T_INT: v.i = val.i; break;
                        case T_UINT: v.ui = (unsigned)val.i; break;
                        case T_FLOAT: {
                            if (t.bits > 32) v.d = (double)val.i;
                            else v.f = (float)val.i;
                        } break;
                        default:
                            throw runtime_error("int literal type mismatch");
                    }
                    break;
                case T_FLOAT:
                    switch (t.type) {
                        case T_INT: v.i = (int)val.d; break;
                        case T_FLOAT:
                            if (t.bits <= 32) v.f = (float)val.d;
                            else v.d = val.d; break;
                        default:
                            throw runtime_error("float literal type mismatch");
                    } break;
                default: break;//throw runtime_error("literal value type mismatch");
            }
            v.type = &t;
            return v;
            //        }
            //        return val;
    }

    static Val parseValRaw(ParseEnv& penv, istream& in, const Token& tok) {
        switch (tok.type) {
            case TOK_VAL: {
                Val val = refineType(tok.val, *parseTypeVal(penv, in));
                return val;
            }
            case TOK_LSQUARE: {
                ArrayVal* av = managed(penv.allocArrayVal);
                for (;;) {
                    Token tok = getToken(in);
                    if (tok.type == TOK_RSQUARE) break;
                    av->elements.push_back(parseValRaw(penv, in, tok));
                }
                const Type* t = parseTypeVal(penv, in);
                //                unsigned sz = t->arr->size;
                // see below
                Val v = {t};
                v.arr = av;
                return v;
            }
            case TOK_LBRACE: {
                StructVal* sv = managed(penv.allocStructVal);
                for (;;) {
                    Token tok = getToken(in);
                    if (tok.type == TOK_RBRACE) break;
                    sv->fields.push_back(parseValRaw(penv, in, tok));
                }
                const Type* t = parseTypeVal(penv, in);
                // todo: check size: filling missing with undef (add sir::undef)
//                 unsigned sz = t->strct->subTypes.size();
//                 if (sv->fields.size() > sz)
                Val v = {t};
                v.strct = sv;
                return v;
            }
            default: throw runtime_error("expected val");
        }
    }

    static const Expr* parseAtomRaw(ParseEnv& penv, istream& in,
                                    const Token& tok) {
        if (tok.type == TOK_IDENT)
            return buildVar(penv, tok.ident);
        else
            return buildConst(penv, parseValRaw(penv, in, tok));
    }

    static const Expr* parseAtom(ParseEnv& penv, istream& in) {
        return parseAtomRaw(penv, in, getToken(in));
    }

    static const Expr* parseExprRaw(ParseEnv&, istream&, const Token&);

    static void parseExprList(ParseEnv& penv, istream& in, ExprList& body) {
        parseSyntax(in, TOK_LBRACE);
        while (in) {
            Token tok = getToken(in);
            if (tok.type == TOK_RBRACE) break;
            body.push_back(parseExprRaw(penv, in, tok));
        }
    }

    static Alt parseAltRaw(ParseEnv& penv, istream& in, const Token& tok) {
        parseSyntaxRaw(in, TOK_LSQUARE, tok);
        int tag = parseInt(in);
        parseSyntax(in, TOK_SEPARATOR);
        ExprList body;
        parseExprList(penv, in, body);
        parseSyntax(in, TOK_RSQUARE);
        return make_pair(tag, body);
    }

//     static const Expr* parseAlt(ParseEnv& penv, istream& in) {
//         return parseAltRaw(penv, in, getToken(in));
//     }

    static const Expr* parseRhs(ParseEnv&, istream&);

    static const Expr* parseRhsRaw(ParseEnv& penv, istream& in,
                                   const Token& tok) {
        switch (tok.type) {
            case TOK_IDENT: case TOK_VAL: case TOK_LSQUARE: case TOK_LBRACE: {
                const Expr* atom = parseAtomRaw(penv, in, tok);
                parseSyntax(in, TOK_ENDSTMT);
                return atom;
            }
            case TOK_CASE: {
                const Type* contType = parseTypeVal(penv, in);
                const Expr* scrut = parseAtom(penv, in);
                CaseExpr* cse = managed(penv.allocCaseExpr);
                cse->contType = contType;
                cse->scrutinee = scrut;
                parseExprList(penv, in, cse->defAlt);
                for (;;) {
                    Token tok = getToken(in);
                    if (tok.type == TOK_ENDSTMT) break;
                    cse->alts.push_back(parseAltRaw(penv, in, tok));
                }
                Expr* e = managed(penv.allocExpr);
                e->type = EXPR_Case;
                e->cse = cse;
                return e;
            }
            case TOK_CAST: {
                const Type* target = parseTypeVal(penv, in);
                const Expr* val = parseRhs(penv, in);
                Expr* e = managed(penv.allocExpr);
                CastExpr* c = managed(penv.allocCastExpr);//managed(penv.allocCastExpr);
                c->type = tok.cast;
                c->target = target;
                c->val = val;
                e->type = EXPR_Cast;
                e->cast = c;
                return e;
            }
            case TOK_OP: { // todo
                OpExpr* o = managed(penv.allocOpExpr);
                o->type = tok.op;
                for (;;) {
                    Token tok = getToken(in);
                    if (tok.type == TOK_ENDSTMT) break;
                    o->args.push_back(parseAtomRaw(penv, in, tok));
                }
                Expr* e = managed(penv.allocExpr);
                e->type = EXPR_Op;
                e->op = o;
                return e;
            }
            case TOK_CALL: {
                Name procName = parseIdent(in);
                SigMap::const_iterator itr = penv.sigs.find(procName);
                if (itr == penv.sigs.end())
                    throw runtime_error("calling unknown proc: " + procName);
                CallExpr* c = managed(penv.allocCallExpr);
                c->proc = itr->second;
                for (;;) {
                    Token tok = getToken(in);
                    if (tok.type == TOK_ENDSTMT) break;
                    c->args.push_back(parseAtomRaw(penv, in, tok));
                }
                if (c->args.size() != c->proc->type->paramTypes.size())
                    throw runtime_error("invalid number of args in call to: "
                                        + procName);
                Expr* e = managed(penv.allocExpr);
                e->type = EXPR_Call;
                e->call = c;
                return e;
            }
            case TOK_TYPEATTR: {
                const Type* tval = parseTypeVal(penv, in);
                parseSyntax(in, TOK_ENDSTMT);
                TypeAttrExpr* tattr = managed(penv.allocTypeAttrExpr);
                tattr->type = tok.tattr;
                tattr->tval = tval;
                Expr* e = managed(penv.allocExpr);
                e->type = EXPR_TypeAttr;
                e->tattr = tattr;
                return e;
            }
            default: throw runtime_error("expected expr rhs");
        }
    }

    static const Expr* parseRhs(ParseEnv& penv, istream& in) {
        return parseRhsRaw(penv, in, getToken(in));
    }

    static const Expr* parseExprRaw(ParseEnv& penv, istream& in,
                                    const Token& tok) {
        if (tok.type == TOK_IDENT) {
            Token next = getToken(in);
            if (next.type == TOK_ASSIGN) {
                const Expr* rhs = parseRhs(penv, in);
                Expr* e = managed(penv.allocExpr);
                AssignExpr* a = managed(penv.allocAssignExpr);
                a->lhs = tok.ident;
                a->rhs = rhs;
                e->type = EXPR_Assign;
                e->assign = a;
                return e;
            } else {
                parseSyntaxRaw(in, TOK_ENDSTMT, next);
                return buildVar(penv, tok.ident);
            }
        } else if (tok.type == TOK_RET) {
            const Expr* rhs;
            Token tok = getToken(in);
            if (tok.type == TOK_ENDSTMT) rhs = 0; // void
            else {
                rhs = parseAtomRaw(penv, in, tok);
                parseSyntax(in, TOK_ENDSTMT);
            }
            Expr* e = managed(penv.allocExpr);
            ReturnExpr* r = managed(penv.allocReturnExpr);
            r->val = rhs;
            e->type = EXPR_Return;
            e->ret = r;
            return e;
        } else return parseRhsRaw(penv, in, tok);
    }

    const Expr* parseExpr(ParseEnv& penv, istream& in) {
        return parseExprRaw(penv, in, getToken(in));
    }

    typedef pair<const ProcType*, NameList> ProcTuple;

    static ProcTuple parseProcTuple(ParseEnv& penv, istream& in,
                                    const Type& retType) {
        parseSyntax(in, TOK_LPAREN);
        NameList paramNames;
        ProcType* pt = managed(penv.allocProcType);
        pt->retType = &retType;
        for (;;) {
            Token tok = getToken(in);
            if (tok.type == TOK_RPAREN) break;
            pt->paramTypes.push_back(parseTypeValRaw(penv, in, tok));
            tok = getToken(in);
            if (tok.type == TOK_SEPARATOR) continue;
            else if (tok.type == TOK_IDENT) {
                paramNames.push_back(tok.ident);
                tok = getToken(in);
                if (tok.type == TOK_SEPARATOR) continue;
                else if (tok.type == TOK_RPAREN) break;
                else throw runtime_error("expected separator or )");
            } else if (tok.type == TOK_RPAREN) break;
            else throw runtime_error("expected ident, separator or )");
        }
        return make_pair(pt, paramNames);
    }

    typedef pair<const ProcSig*, NameList> SigProcTuple;

    // sig int blah(int, float);
    static SigProcTuple parseSig(ParseEnv& penv, istream& in) {
        const Type* ret = parseTypeVal(penv, in);
        Name name = parseIdent(in);
        ProcTuple pt = parseProcTuple(penv, in, *ret);
        ProcSig* sig = managed(penv.allocSig);
        sig->val.name = name;
        sig->type = pt.first;
        penv.sigs[name] = sig; // todo: check if it exists first
        return make_pair(sig, pt.second);
    }

    // def int blah(int a, float b) {
    //   ## body
    // }
    static const ProcDef* parseDef(ParseEnv& penv, istream& in) {
        SigProcTuple sigTup = parseSig(penv, in);
        // check arg names vs. types
        if (sigTup.second.size() != sigTup.first->type->paramTypes.size())
            throw runtime_error("all def params must be named");
        ProcDef* def = managed(penv.allocDef);
        def->paramNames = sigTup.second;
        def->sig = sigTup.first;
        parseExprList(penv, in, def->body);
        penv.defs[sigTup.first->val.name] = def; // todo: check if it exists first
        return def;
    }

    TopVal parseTop(ParseEnv& penv, istream& in) {
        Token tok = getToken(in);
        TopVal top;
        switch (tok.type) {
            case TOK_TYPE:
                top.type = TOP_TYPE;
                top.t = parseType(penv, in);
                break;
            case TOK_SIG: {
                top.type = TOP_SIG;
                SigProcTuple spt = parseSig(penv, in);
                parseSyntax(in, TOK_ENDSTMT);
                top.sig = spt.first;
            } break;
            case TOK_DEF:
                top.type = TOP_DEF;
                top.def = parseDef(penv, in);
                break;
            case TOK_EOF:
                top.type = TOP_EOF;
                break;
            default:
                top.type = TOP_EXPR;
                top.expr = parseExprRaw(penv, in, tok);
                break;
        }
        return top;
    }

    template <typename ListType>
    void freeList(const ListType& lst) {
        typename ListType::const_iterator itr = lst.begin(), end = lst.end();
        for (; itr != end; ++itr)
            delete *itr;
    }

    void freeParseEnv(ParseEnv& penv) { // todo: frequent freeing to save space
        freeList(penv.allocSig);
        freeList(penv.allocDef);
        freeList(penv.allocArrayVal);
        freeList(penv.allocStructVal);
        freeList(penv.allocType);
        freeList(penv.allocArrayType);
        freeList(penv.allocStructType);
        freeList(penv.allocExpr);
        freeList(penv.allocConstExpr);
        freeList(penv.allocVarExpr);
        freeList(penv.allocAssignExpr);
        freeList(penv.allocCaseExpr);
        freeList(penv.allocCastExpr);
        freeList(penv.allocOpExpr);
        freeList(penv.allocCallExpr);
        freeList(penv.allocReturnExpr);
        freeList(penv.allocProcType);
    }

    static bool initialized = false;

    static void init() {
        if (initialized) return;
        initialized = true;
        keywords["type"] = TOK_TYPE;
        keywords["sig"] = TOK_SIG;
        keywords["def"] = TOK_DEF;
        keywords["call"] = TOK_CALL;
        keywords["case"] = TOK_CASE;
        keywords["ret"] = TOK_RET;
        keywords["global"] = TOK_GLOBAL;
        keywords["const"] = TOK_CONST;
        syntax['('] = TOK_LPAREN;
        syntax[')'] = TOK_RPAREN;
        syntax['{'] = TOK_LBRACE;
        syntax['}'] = TOK_RBRACE;
        syntax['['] = TOK_LSQUARE;
        syntax[']'] = TOK_RSQUARE;
        syntax['<'] = TOK_LANGLE;
        syntax['>'] = TOK_RANGLE;
        syntax['@'] = TOK_PTR;
        syntax[','] = TOK_SEPARATOR;
        syntax[';'] = TOK_ENDSTMT;
        syntax['='] = TOK_ASSIGN;
        syntax[':'] = TOK_COMMAND; // :q, :i, :dump, etc...
        casts["resize"] = CAST_RESIZE;
        casts["convert"] = CAST_CONVERT;
        casts["reinterpret"] = CAST_REINTERP;
        typeAttrs["sizeof"] = TYPEATTR_SIZEOF;
        ops["select"] = OP_SELECT;
        ops["extract"] = OP_EXTRACT;
        ops["insert"] = OP_INSERT;
        ops["neg"] = OP_NEG;
        ops["not"] = OP_NOT;
        ops["load"] = OP_LOAD;
        ops["add"] = OP_ADD;
        ops["sub"] = OP_SUB;
        ops["mul"] = OP_MUL;
        ops["div"] = OP_DIV;
        ops["rem"] = OP_REM;
        ops["shl"] = OP_SHL;
        ops["shr"] = OP_SHR;
        ops["and"] = OP_AND;
        ops["or"] = OP_OR;
        ops["xor"] = OP_XOR;
        ops["eq"] = OP_EQ;
        ops["neq"] = OP_NEQ;
        ops["lt"] = OP_LT;
        ops["lteq"] = OP_LTEQ;
        ops["gt"] = OP_GT;
        ops["gteq"] = OP_GTEQ;
        ops["offset"] = OP_OFFSET;
        ops["store"] = OP_STORE;
        //        ops[""] = OP_;
    }

    void initParseEnv(ParseEnv& penv) {
        init();
        penv.types["void"] = &voidType;
        penv.types["bool"] = &boolType;
        penv.types["char"] = &charType;
        penv.types["byte"] = &byteType;
        penv.types["int"] = &intType;
        penv.types["uint"] = &uintType;
        penv.types["float"] = &floatType;
        penv.types["double"] = &doubleType;
    }
}
