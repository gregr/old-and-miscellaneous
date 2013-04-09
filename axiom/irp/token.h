#ifndef irp_token_H_
#define irp_token_H_

#include <istream>
#include <ostream>
#include <map>
#include <set>
#include <string>

namespace irp {

    enum TokenType { TOKEN_EOF, TOKEN_Syntax, TOKEN_MetaSymbol, TOKEN_Symbol,
                     TOKEN_Int, TOKEN_Float, TOKEN_Char, TOKEN_String };

    struct StrRef {
        const std::string* val;
    };

    struct Token {
        //        std::string s; // can't place in union
        TokenType type;
        int row, col;
        union {
            StrRef* s;
            int i;
            double f;
            char c;
        };
    };

    struct TokenContext {
        std::set<char> reserved;
        typedef std::map<std::string, StrRef> StringMap;
        StringMap strings;
    };

    StrRef* internString(TokenContext&, const std::string&);
    Token getToken(TokenContext&, std::istream&);
    void showToken(std::ostream&, const Token&);
}

#endif
