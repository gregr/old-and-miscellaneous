#ifndef irp_parse_H_
#define irp_parse_H_

#include "token.h"

namespace irp {

    struct ParseContext;
    typedef (void*)(Parser*)(ParseEnv&, std::istream&, const Token&);

    struct ParseContext {
        TokenContext tctx;
//         std::string openBrackets, closeBrackets;
//         Parser onApp, onAtom;
    };

    struct ParseEnv {
        Env<TokenType, Parser> topParsers;
        Env<char, Parser> syntaxParsers;
        Env<StrRef*, Parser> metaParsers;
        Env<StrRef*, Parser> macroParsers;
        Env<StrRef*, void*> bindings;
        ParseContext* ctx;
    };

    void initBrackets(ParseContext&, const std::string&, const std::string&);
    void initMacros(ParseEnv&, const std::map<std::string, Parser>&);
    void* parse(ParseEnv&, std::istream&);
}

#endif
