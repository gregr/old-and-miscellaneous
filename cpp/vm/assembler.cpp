#include "machine.h"
#include "ops.h"
#include "boost/function.hpp"
#include "boost/regex.hpp"
#include "boost/lexical_cast.hpp"
#include "boost/format.hpp"
#include <map>
#include <list>
#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include <stdexcept>
using namespace std;
using namespace boost;

const regex reLabel("[a-zA-Z_]\\w*:");
const regex reSymbol("[$*+&%#@!~^]");

const regex reIdent("[a-zA-Z_]\\w*");
const regex reInt("-?(0x)?\\d+\\b");
const regex reFloat("-?(\\d*\\.?\\d+)([eE]\\d+)?");
const regex reChar("'(\\\\.|[^\\\\\'])'");
//const regex reString("\"(\\\\.|[^\\\\\"])*\""); // need data regions first

const char commentStart = ';';

bool isBoundary(char c) { return isspace(c) || c == commentStart; }

bool isComment(const string& s) {
    return !s.empty() && s[0] == commentStart;
}

pair<char, bool> evalChar(char current, char next) {
    char c;
    bool escaped = false;
    if (current == '\\') {
        escaped = true;
        if (next == '0')
            c = '\0';
        else if (next == 't')
            c = '\t';
        else if (next == 'v')
            c = '\v';
        else if (next == 'n')
            c = '\n';
        else if (next == 'r')
            c = '\r';
        else
            c = next;
    }
    else
        c = current;
    return make_pair(c, escaped);
}

struct Value {
    enum ValueType { NONE, LABEL, ACTUAL };
    Value() : type(NONE) {}
    Value(word v) : type(ACTUAL), actual(v) {}
    Value(const string& l) : type(LABEL), label(l) {}

    bool isNone() const { return type == NONE; }

    ValueType type;
    string label, prefix;
    word actual;
};

Value convertChar(const string& s) {
    // s[0] should be '
    pair<char, bool> result = evalChar(s[1], s[2]);
    if (result.second && s.length() != 4)
        ; // murder
    return Value((word)(result.first));
}

Value convertIdent(const string& s) { return Value(s); }

Value convertLabel(const string& s) { return Value(s.substr(0, s.length()-1)); }

Value convertInteger(const string& s) {
    return Value((word)lexical_cast<int>(s));
}

Value convertFloat(const string& s) {
    float val = lexical_cast<float>(s);
    return Value(*((word*)&val));
}

// template <typename LiteralType>
// Value convertLiteral(const string& s) {
//     LiteralType val = lexical_cast<LiteralType>(s);
//     return Value(*((word*)&val));
// }

string lstrip(const string& s) {
    size_t ix = 0, end = s.length();
    for (; ix != end; ++ix)
        if (!isspace(s[ix]))
            break;
    return s.substr(ix);
}

string nextStart(const string& s) {
    string start = lstrip(s);
    if (isComment(start))
        return "";
    return start;
}

typedef function<Value (string s)> ConvertFunc;
typedef pair<regex, ConvertFunc> Regexp;
typedef vector<Regexp> RegexpList;

pair<Value, string> matchAgainst(const string& line, const RegexpList& regexps,
                                 bool prefix=false) {
    string s = nextStart(line);
    match_results<string::const_iterator> m;
    RegexpList::const_iterator re = regexps.begin(),
        end = regexps.end();
    for (; re != end; ++re) {
        if (regex_search(s, m, re->first, match_continuous))
            if (prefix || m[0].length() == s.length() ||
                isBoundary(s[m[0].length()]))
                return make_pair(re->second(m[0].str()),
                                 s.substr(m[0].length()));
    }
    return make_pair(Value(), s);
}

// can't use a function due to sizeof
#define REGEXP_LIST(name, lst)\
    const RegexpList name(lst, lst+sizeof(lst)/sizeof(Regexp))

Regexp rInitial_[] = {
    make_pair(reLabel, convertLabel),
};

Regexp rOperator_[] = {
    make_pair(reIdent, convertIdent),
};

Regexp rSymbol_[] = {
    make_pair(reSymbol, convertIdent),
};

Regexp rOperands_[] = {
    make_pair(reIdent, convertIdent),
    make_pair(reInt, convertInteger),
    make_pair(reFloat, convertFloat),
    make_pair(reChar, convertChar),
};

REGEXP_LIST(resInitial, rInitial_);
REGEXP_LIST(resOperator, rOperator_);
REGEXP_LIST(resSymbol, rSymbol_);
REGEXP_LIST(resOperands, rOperands_);

typedef vector<Value> Instr;

string valueToString(const Value& val) {
    if (val.isNone())
        return "NONE";
    else if (val.type == Value::LABEL)
        return val.prefix+val.label;
    else if (val.type == Value::ACTUAL)
        return val.prefix+lexical_cast<string>(val.actual);
    return "error: valueToString";
}

string instrToString(const Instr& instr) {
    string result = "[ ";
    Instr::const_iterator ix = instr.begin(), end = instr.end();
    for (; ix != end; ++ix)
        result += valueToString(*ix)+", ";
    return result+']';
}

void raiseParseError(const string& msg, const string& line, const string& rest,
                     const Instr& instr) {
    throw runtime_error(msg + (format(": %d, %s, %s")
                               % (line.length()-rest.length())
                               % line % instrToString(instr)).str());
}

typedef pair<Value, Instr> LabeledInstr;

LabeledInstr parseLine(const string& line) {
    Instr instr;
    pair<Value, string> result = matchAgainst(line, resInitial);
    Value label = result.first;
    const string& rest = result.second;
    if (!label.isNone() && label.type != Value::LABEL)    
        raiseParseError("label is not a string? wtf!", line, rest, instr);
    if (!label.isNone() || !rest.empty()) {
        result = matchAgainst(rest, resOperator);
        Value op = result.first;
        if (!op.isNone() || !rest.empty()) {
            instr.push_back(op);
            while (!rest.empty()) {
                if (op.isNone())
                    raiseParseError("error while parsing", line, rest, instr);
                result = matchAgainst(rest, resSymbol, true);
                if (!result.first.isNone() && result.first.type != Value::LABEL)
                    raiseParseError("prefix is not a string? wtf!",
                                    line, rest, instr);
                string prefix = result.first.label;
                result = matchAgainst(rest, resOperands);
                op = result.first;
                op.prefix = prefix;
                if (!op.isNone())
                    instr.push_back(op);
            }
        }
    }
    return make_pair(label, instr);
}

typedef vector<LabeledInstr> LabeledProgram;

LabeledProgram parseProg(istream& in) {
    LabeledProgram prog;
    string line;
    while (getline(in, line)) {
        LabeledInstr instr = parseLine(line);
        if (!instr.first.isNone() || !instr.second.empty())
            prog.push_back(instr);
    }
    return prog;
}

typedef map<string, unsigned> LabelMap;

LabelMap scanLabels(const LabeledProgram& prog) {
    LabelMap labels;
    unsigned current = 0;
    LabeledProgram::const_iterator ix = prog.begin(), end = prog.end();
    for (; ix != end; ++ix) {
        if (!ix->first.isNone()) {
            //            cout << "label: " << ix->first.label << " at: " << current << endl;
            if (!labels.insert(make_pair(ix->first.label, current)).second)
                throw runtime_error("duplicate label: " + ix->first.label);
        }
        if (!ix->second.empty()) {
            ++current;
            //            cout << "current: " << current << endl;
        }
    }
    return labels;
}

int getOpcode(const Value& op) {
    if (op.type != Value::LABEL)
        throw runtime_error("op is not a name");
    int opcode = opByName("op_"+op.label);
    if (opcode == -1)
        throw runtime_error("invalid op name: " + op.label);
    return opcode;
}

const string regPrefix = "%";
const string labelPrefix = "$";

word evalOperand(const LabelMap& labels, const Value& val) {
    if (val.isNone())
        throw runtime_error("evaluated operand with value NONE");
    if (val.prefix == labelPrefix) {
        assert(val.type == Value::LABEL);
        LabelMap::const_iterator itr = labels.find(val.label);
        if (itr == labels.end())
            throw runtime_error("invalid label");
        return itr->second;
    } else if (val.prefix == regPrefix) {
        assert(val.type == Value::ACTUAL);
        return val.actual;
    } else {
        assert(val.type == Value::ACTUAL);
        return val.actual;
    }
}

vector<word> assemble(const LabeledProgram& prog) {
    LabelMap labels = scanLabels(prog);
    vector<word> code;
    LabeledProgram::const_iterator ix = prog.begin(), end = prog.end();
    for (; ix != end; ++ix) {
        const Instr& instr = ix->second;
        if (!instr.empty()) {
            word ins = 0;
            byte* cur = (byte*)&ins;
            switch (instr.size()) {
                case 4:
                    cur[2] = evalOperand(labels, instr[2]);
                    cur[3] = evalOperand(labels, instr[3]);
                case 3:
                    if (instr.size() == 3) {
                        *((byte2*)(cur+2)) = evalOperand(labels, instr[2]);
                    }
                case 2:
                    cur[1] = evalOperand(labels, instr[1]);
                case 1:
                    cur[0] = (byte)getOpcode(instr[0]);
                    break;
                default:
                    throw runtime_error("invalid instruction size");
            }
            code.push_back(ins);
        }
    }
    return code;
}

template <typename T>
void serialize(ostream& out, const T& value) {
    out.write(reinterpret_cast<const char*>(&value), sizeof(value));
}

template <typename T>
void deserialize(istream& in, T& value) {
    in.read(reinterpret_cast<char*>(&value), sizeof(value));
}

void serializeCode(ostream& out, const vector<word>& code) {
    size_t len = code.size();
    serialize(out, len);
    vector<word>::const_iterator ix = code.begin(), end = code.end();
    for (; ix != end; ++ix)
        serialize(out, *ix);
}

vector<word> deserializeCode(istream& in) {
    size_t len;
    deserialize(in, len);
    vector<word> code;
    code.reserve(len);
    word instr;
    for (size_t ix = 0; ix < len; ++ix) {
        deserialize(in, instr);
        code.push_back(instr);
    }
    return code;
}

void printCode(ostream& out, const vector<word>& code) {
    cout << "length: " << code.size() << endl;
    vector<word>::const_iterator ix = code.begin(), end = code.end();
    for (; ix != end; ++ix)
        cout << *ix << endl;
}

vector<word> assembleProg(istream& in) {
    return assemble(parseProg(in));
}

// const RegexpList resInitial(rInitial_, rInitial+sizeof(rInitial_)/sizeof(Regexp)),
//     resOperator, resSymbol, resOperands;

// int main() {
//     try {
//         ifstream inputFile("fact.asm");
//         LabeledProgram prog = parseProg(inputFile);
//         vector<word> code = assemble(prog);
//         cout << "before" << endl;
//         printCode(cout, code);
//         {
//             ofstream fout("fact.code", ios_base::out|ios_base::binary);
//             //    stringstream ss;
//             serializeCode(fout, code);
//         }
//         ifstream fin("fact.code", ios_base::in|ios_base::binary);
//         vector<word> code2 = deserializeCode(fin);
//         cout << "\nafter" << endl;
//         printCode(cout, code2);

//         prettyProg(cout, &code[0], code.size());
//         State s;
//         run(s, &code[0]);
//         //        cout << "result: " << s.wordRegs[2] << endl;
//         s.view(cout);
// //     // seeing if op_muld actually works
// //     word a = s.wordRegs[2];
// //     word b = s.wordRegs[3];
// //     cout << a << endl;
// //     cout << b << endl;
// //     cout << *((dword*)(&s.wordRegs[0])) << endl;
// //     dword result = (dword)a * b;
// //     cout << result << endl;

//     } catch (const std::exception& e) {
//         cerr << "exception: " << e.what() << endl;
//     }

//     //    cout << opByName("op_halt") << endl;
//     //    function<Value (string s)> f = &convertLiteral<double>;
// //     string content = "woohoo: fwej %325 $a4hello";
// //     pair<Value, Instr> result = parseLine(content);
// //     cout << valueToString(result.first) << endl;
// //     cout << instrToString(result.second) << endl;
// //     match_results<string::const_iterator> m;
// //     regex_search(content, m, reLabel, match_continuous);
// //     cout << m[0].matched << ' ' << m[0].length() << ' ' << m[0].str() << endl;
//     //    cout << f("4.2").actual << endl;
// //     pair<string, string> result = getMatch(content, reIdent);
// //     cout << result.first << endl;
// //     cout << result.second << endl;
// //     match_results<string::const_iterator> m;
// //     bool result = regex_search(content, m, reIdent, match_continuous);
// //     if (result) {
// //         cout << "matched: " << m[0].str() << endl;
// //     }
// //     else
// //         cout << "no match! " << m[0].str() << endl;
//     return 0;
// }
