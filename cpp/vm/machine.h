#include <ostream>
#include <iomanip>
#include <vector>

typedef long long sdword;
typedef unsigned long long dword;
typedef signed sword;
typedef unsigned word;
typedef char sbyte;
typedef unsigned char byte;
typedef short sbyte2;
typedef unsigned short byte2;
typedef sword sbyte4;
typedef word byte4; // assuming 32-bit words
typedef sdword sbyte8;
typedef dword byte8;

const unsigned TERMINAL_WIDTH = 80;

void prettyProg(std::ostream& out, const word* prog, unsigned len);

template<typename T>
void viewMem(std::ostream& out, const T* mem, size_t len,
             size_t width=TERMINAL_WIDTH) {
    const unsigned int hexWidth = sizeof(T)*2;
    const unsigned int columns = width / (hexWidth+1);
    if (columns == 0) {
        out << "view width too narrow" << std::endl;
        return;
    }
    char prevFill = out.fill('0');
    std::ios_base::fmtflags flags = out.flags();
    out << std::hex;
    int column = 0;
    for (int i=0; i < len; ++i, ++column) {
        if (column == columns) {
            column = 0;
            out << std::endl;
        }
        out << std::setw(hexWidth) << mem[i] << ' ';
    }
    out << std::endl;
    out.fill(prevFill); // it's not really exception safe, but...
    out.flags(flags);
}

template<typename T>
void viewRegs(std::ostream& out, const std::vector<T>& regs,
              const std::string& name) {
    out << name << " registers:" << std::endl;
    viewMem(out, &regs[0], regs.size());
}

struct State {
    State(size_t numRegs=16) { resizeRegs(numRegs); }
    void resizeRegs(size_t numRegs) {
        wordRegs.resize(numRegs);
        floatRegs.resize(numRegs);
    }
    void view(std::ostream& out) const {
        viewRegs(out, wordRegs, "word");
        viewRegs(out, floatRegs, "double-precision float");
    }
    std::vector<word> wordRegs;
    std::vector<double> floatRegs;
};

void run(State& state, const word* instrs);
void run(State& state, const std::vector<word>& instrs);
