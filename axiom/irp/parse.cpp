#include "parse.h"

namespace irp {

    static const char beginExpr = '(';
    static const char endExpr = ')';
    static const char beginList = '[';
    static const char endList = ']'
    static const char beginSet = '{';
    static const char endSet = '}';

    void initBrackets(ParseContext& ctx) {
        ctx.openBrackets = openBrackets;
        ctx.closeBrackets = closeBrackets;
        string::const_iterator itr = openBrackets.begin(),
            end = openBrackets.end();
        for (; itr != end; ++itr)
            ctx.tctx.reserved.insert(*itr);
        itr = closeBrackets.begin(), end = closeBrackets.end();
        for (; itr != end; ++itr)
            ctx.tctx.reserved.insert(*itr);
    }

    void initMacros(ParseEnv& p, const map<string, ParseMacro>& pmacs) {
        map<string, ParseMacro>::const_iterator itr = pmacs.begin(),
            end = pmacs.end();
        for (; itr != end; ++itr)
            p.parseMacros.add(internString(penv.ctx->tctx, itr->first),
                                 itr->second);
    }

    void initExprSyntax(ParseEnv& penv, char beginExpr='(', char endExpr=')') {
    }

    void initTopParsers(ParseEnv& penv) {
        penv.topParsers.add(TOKEN_EOF, parseEOF);
        penv.topParsers.add(TOKEN_Syntax, parseSyntax);
        penv.topParsers.add(TOKEN_MetaSymbol, parseMetaSymbol);
    }

    void* parseEOF(ParseEnv& penv, istream& in, const Token& tok) {
        return 0;
    }

    void* parseSyntax(ParseEnv& penv, istream& in, const Token& tok) {
        return (*penv.syntaxParsers.get(tok.c))(penv, in, tok);
    }

    void* parseMetaSymbol(ParseEnv& penv, istream& in, const Token& tok) {
        return (*penv.metaParsers.get(tok.s))(penv, in, tok);
    }

    void* parseExpr(ParseEnv& penv, istream& in, const Token& tok) {
    }

    void* parse(ParseEnv& penv, istream& in) {
        Token tok = getToken(in);
        return (*penv.topParsers.get(tok.type))(penv, in, tok);
    }
}
