#include "token.h"
#include <stdexcept>
using namespace std;

namespace irp {

    static char getNoEOF(istream& in) {
        if (!in) throw runtime_error("unexpected eof");
        return in.get();
    }

    static void skipSpace(istream& in) {
        while (in) {
            char ch = in.get();
            if (!isspace(ch)) {
                in.putback(ch);
                break;
            }
        }
    }

    static void skipLine(istream& in) {
        while (in) {
            char ch = in.get();
            if (ch == '\n') break;
            else if (ch == '\\') getNoEOF(in);
        }
    }

    static Token getNumeric(istream& in, bool negative) { // todo: corner cases
        Token tok;
        string num;
        if (negative) num += '-';
        bool isFloat = false, foundExp = false;
        while (in) {
            char ch = in.get();
            if (!isdigit(ch)) {
                if (ch == '.' && !isFloat) {
                    isFloat = true;
                    num += ch;
                } else if (!foundExp && (ch == 'e' || ch == 'E')) {
                    foundExp = true;
                    isFloat = true;
                    num += ch;
                    char ch = in.get();
                    if (ch == '+' || ch == '-') num += ch;
                    else in.putback(ch);
                } else {
                    in.putback(ch);
                    break;
                }
            } else num += ch;
        }
        if (isFloat) {
            tok.type = TOKEN_Float;
            tok.f = atof(num.c_str());
        } else {
            tok.type = TOKEN_Int;
            tok.i = atoi(num.c_str());
        }
        return tok;
    }

    static char escapeChar(char ch) {
        if (ch == 'n') return '\n';
        else if (ch == 'r') return '\r';
        else if (ch == 't') return '\t';
        else if (ch == 'v') return '\v';
        else if (ch == '0') return '\0';
        else return ch;
    }

    static char maybeEscaped(istream& in, char ch) {
        if (ch == '\\') ch = escapeChar(getNoEOF(in));
        return ch;
    }

    static Token getChar(istream& in) {
        char ch = getNoEOF(in);
        if (getNoEOF(in) != '\'')
            throw runtime_error("expected ' while reading character literal");
        Token tok;
        tok.type = TOKEN_Char;
        tok.c = maybeEscaped(in, ch);
        return tok;
    }

    static Token getString(TokenContext& ctx, istream& in) {
        string s;
        for (;;) {
            char ch = getNoEOF(in);
            if (ch == '"') break;
            s += maybeEscaped(in, ch);
        }
        Token tok;
        tok.type = TOKEN_String;
        tok.s = internString(ctx, s);
        return tok;
    }

    static bool isReserved(TokenContext& ctx, char ch) {
        return ctx.reserved.find(ch) != ctx.reserved.end();
    }

    static Token getSymbol(TokenContext& ctx, istream& in) {
        string sym;
        while (in) {
            char ch = in.get();
            if (isspace(ch) || isReserved(ctx, ch)) {
                in.putback(ch);
                break;
            }
            sym += maybeEscaped(in, ch);
        }
        Token tok;
        tok.type = TOKEN_Symbol;
        tok.s = internString(ctx, sym);
        return tok;
    }


    static Token getMetaSymbol(TokenContext& ctx, istream& in) {
        Token tok = getSymbol(ctx, in);
        tok.type = TOKEN_MetaSymbol;
        return tok;
    }

    static Token getSyntax(char ch) {
        Token tok;
        tok.type = TOKEN_Syntax;
        tok.c = ch;
        return tok;
    }

    StrRef* internString(TokenContext& ctx, const string& s) {
        TokenContext::StringMap::iterator itr = ctx.strings.find(s);
        if (itr == ctx.strings.end()) {
            itr = ctx.strings.insert(make_pair(s, StrRef())).first;
            itr->second.val = &itr->first;
        }
        return &itr->second;
    }

    Token getToken(TokenContext& ctx, istream& in) {
        for (;;) {
            skipSpace(in);
            if (!in) {
                Token tok;
                tok.type = TOKEN_EOF;
                return tok;
            }
            char ch = in.get();
            if (isReserved(ctx, ch)) return getSyntax(ch);
            else if (ch == '#') {
                ch = getNoEOF(in);
                if (ch == '#') skipLine(in); // comment
                else if (isspace(ch) || isReserved(ctx, ch)) {
                    in.putback(ch);
                    return getSyntax('#');
                } else { in.putback(ch); return getMetaSymbol(ctx, in); }
            } else if (isdigit(ch)) {
                in.putback(ch);
                return getNumeric(in, false);
            } else if (ch == '-') {
                if (in) {
                    ch = in.get();
                    if (isdigit(ch)) {
                        in.putback(ch);
                        return getNumeric(in, true);
                    } else {
                        in.putback(ch);
                        in.putback('-');
                        return getSymbol(ctx, in);
                    }
                } else {
                    in.putback('-');
                    return getSymbol(ctx, in);
                }
            } else if (ch == '\'') return getChar(in);
            else if (ch == '"') return getString(ctx, in);
            else if (ch == '\\') {
                ch = getNoEOF(in);
                if (ch != '\n') { // otherwise skip
                    in.putback(ch);
                    in.putback('\\');
                    return getSymbol(ctx, in);
                }
            }
            else {
                in.putback(ch);
                return getSymbol(ctx, in);
            }
        }
    }

    void showToken(ostream& out, const Token& t) {
        switch (t.type) {
            case TOKEN_EOF: out << "<EOF>"; break;
            case TOKEN_Syntax: out << "<Syntax " << t.c << '>'; break;
            case TOKEN_MetaSymbol: out << "<MetaSymbol " << *t.s->val << '>';
                break;
            case TOKEN_Symbol: out << "<Symbol " << *t.s->val << '>'; break;
            case TOKEN_Int: out << "<Int " << t.i << '>'; break;
            case TOKEN_Float: out << "<Float " << t.f << '>'; break;
            case TOKEN_Char: out << "<Char '" << t.c << "'>"; break;
            case TOKEN_String: out << "<String \"" << *t.s->val << "\">"; break;
        }
    }
}
