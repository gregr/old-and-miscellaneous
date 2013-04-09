#include <iostream>
#include <iomanip>
using namespace std;

enum InstrType {
    op_load, op_store, op_lch, op_lcl,
    op_swap,
    op_shl, op_shr, op_shru, op_shli, op_shri, op_shrui,
    op_add, op_sub, op_mul, op_div, op_mod,
    op_addi, op_subi, op_muli, op_divi, op_modi,
    op_fadd, op_fsub, op_fmul, op_fdiv,
    op_faddi, op_fsubi, op_fmuli, op_fdivi,
    op_eq, op_gte, op_gt, op_eqi, op_gtei, op_gti,
    op_and, op_or, op_nor, op_xor,
    op_jez, op_jnz, op_j, op_jezi, op_jnzi, op_ji,
    op_cin, op_cout, op_couti,
    op_itof, op_ftoi,
    op_nop, op_halt
};

typedef size_t word;
typedef unsigned short hword;
typedef unsigned char byte;
typedef word Instruction;

inline word getWord(byte* mem, size_t offset) {
    return ((word*)mem)[offset];
}

inline void setWord(byte* mem, size_t offset, word value) {
    ((word*)mem)[offset] = value;
}

inline void setWordHigh(byte* mem, size_t offset, hword value) {
    *(((hword*)(((word*)mem)+offset))+1) = value;
}

inline void setWordLow(byte* mem, size_t offset, hword value) {
    *(((hword*)(((word*)mem)+offset))+0) = value;
}

inline float getFloat(byte* mem, size_t offset) {
    return ((float*)mem)[offset];
}

inline void setFloat(byte* mem, size_t offset, float value) {
    setWord(mem, offset, *((word*)&value));
}

inline void printMem(word* mem, size_t len, size_t width=8) {
    int column = 0;
    for (int i=0; i < len; ++i, ++column) {
        if (column == width) {
            column = 0;
            cout << endl;
        }
        cout << hex << setfill('0') << setw(8) << (word)mem[i] << ' ';
    }
    cout << endl;
}

inline void jumpi(byte* mem, const Instruction* ins, const Instruction** ip,
                  size_t retReg, size_t dstAddr) {
    setWord(mem, retReg, (word)(*ip - ins));
    *ip = ins + dstAddr;
}

inline void jump(byte* mem, const Instruction* ins, const Instruction** ip,
                 size_t retReg, size_t dstReg) {
    jumpi(mem, ins, ip, retReg, getWord(mem, dstReg));
}

void run(byte* mem, size_t memSize, const Instruction* ins) {
    setWord(mem, 0, (size_t)mem+memSize);
    word tmp;
    byte* current;
    InstrType instrType;
    const Instruction* ip = ins;
    while (ip != 0) {
        current = (byte*)ip;
        instrType = (InstrType)current[0];
        ++ip;
        switch (instrType) {
            case op_load:
                setWord(mem, current[1], getWord(mem, current[2]));
                break;
            case op_store:
                setWord(mem, getWord(mem, current[2]),
                        getWord(mem, current[1]));
                break;
            case op_lch:
                setWordHigh(mem, current[1], *((hword*)(current+2)));
                break;
            case op_lcl:
                setWordLow(mem, current[1], *((hword*)(current+2)));
                break;
            case op_swap:
                tmp = getWord(mem, getWord(mem, current[2]));
                setWord(mem, getWord(mem, current[2]),
                        getWord(mem, current[1]));
                setWord(mem, current[1], tmp);
                break;
            case op_add:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) + getWord(mem, current[3])));
                break;
            case op_sub:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) - getWord(mem, current[3])));
                break;
            case op_mul:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) * getWord(mem, current[3])));
                break;
            case op_div:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) / getWord(mem, current[3])));
                break;
            case op_mod:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) % getWord(mem, current[3])));
                break;
            case op_addi:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) + current[3]));
                break;
            case op_subi:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) - current[3]));
                break;
            case op_muli:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) * current[3]));
                break;
            case op_divi:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) / current[3]));
                break;
            case op_modi:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) % current[3]));
                break;
            case op_fadd:
                setFloat(mem, current[1],
                         getFloat(mem, current[2]) + getFloat(mem, current[3]));
                break;
            case op_fsub:
                setFloat(mem, current[1],
                         getFloat(mem, current[2]) - getFloat(mem, current[3]));
                break;
            case op_fmul:
                setFloat(mem, current[1],
                         getFloat(mem, current[2]) * getFloat(mem, current[3]));
                break;
            case op_fdiv:
                setFloat(mem, current[1],
                         getFloat(mem, current[2]) / getFloat(mem, current[3]));
                break;
                // todo
//             case op_faddi:
//                 setFloat(mem, current[1],
//                          getFloat(mem, current[2]) + getFloat(mem, current[3]));
//                 break;
//             case op_fsubi:
//                 break;
//             case op_fmuli:
//                 break;
//             case op_fdivi:
//                 break;
            case op_shl:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) << getWord(mem, current[3])));
                break;
            case op_shr: // todo
                setWord(mem, current[1],
                        (getWord(mem, current[2]) >> getWord(mem, current[3])));
                break;
            case op_shru:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) >> getWord(mem, current[3])));
                break;
            case op_shli:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) << current[3]));
                break;
            case op_shri: // todo
                setWord(mem, current[1],
                        (getWord(mem, current[2]) >> current[3]));
                break;
            case op_shrui:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) >> current[3]));
                break;
            case op_eq:
                setWord(mem, current[1],
                        (word)(getWord(mem, current[2]) ==
                               getWord(mem, current[3])));
                break;
            case op_gte:
                setWord(mem, current[1],
                        (word)(getWord(mem, current[2]) >=
                               getWord(mem, current[3])));
                break;
            case op_gt:
                setWord(mem, current[1],
                        (word)(getWord(mem, current[2]) >
                               getWord(mem, current[3])));
                cout << "gt: " << getWord(mem, current[2]) << " " <<
                    getWord(mem, current[3]) << " " <<
                    (getWord(mem, current[2]) > getWord(mem, current[3]))
                     << endl;
                break;
            case op_eqi:
                setWord(mem, current[1],
                        (word)(getWord(mem, current[2]) == current[3]));
                break;
            case op_gtei:
                setWord(mem, current[1],
                        (word)(getWord(mem, current[2]) >= current[3]));
                break;
            case op_gti:
                setWord(mem, current[1],
                        (word)(getWord(mem, current[2]) > current[3]));
                break;
            case op_and:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) & getWord(mem, current[3])));
                break;
            case op_or:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) | getWord(mem, current[3])));
                break;
            case op_nor:
                setWord(mem, current[1],
                        ~(getWord(mem, current[2]) | getWord(mem, current[3])));
                break;
            case op_xor:
                setWord(mem, current[1],
                        (getWord(mem, current[2]) ^ getWord(mem, current[3])));
                break;
            case op_jez:
                if (getWord(mem, current[2]) == 0)
                    jump(mem, ins, &ip, current[1], current[3]);
                break;
            case op_jezi:
                if (getWord(mem, current[2]) == 0)
                    jumpi(mem, ins, &ip, current[1], current[3]);
                break;
            case op_jnz:
                if (getWord(mem, current[2]) != 0)
                    jump(mem, ins, &ip, current[1], current[3]);
                break;
            case op_jnzi:
                if (getWord(mem, current[2]) != 0)
                    jumpi(mem, ins, &ip, current[1], current[3]);
                break;
            case op_j:
                jump(mem, ins, &ip, current[1], current[2]);
                break;
            case op_ji:
                jumpi(mem, ins, &ip, current[1], current[2]);
                break;
            case op_cin:
                break;
            case op_cout:
                cout << (char*)((word*)mem + getWord(mem, current[1]));
                break;
            case op_couti:
                cout << (char)current[1];
                break;
            case op_itof:
                setFloat(mem, current[1], (float)getWord(mem, current[1]));
                break;
            case op_ftoi:
                setWord(mem, current[1], (word)getFloat(mem, current[1]));
                break;
            case op_nop:
                break;
            case op_halt:
                ip = 0;
                break;
        }
    }
}

byte test[] = {
    (byte)op_lcl, 1, 20, 0,
    (byte)op_lcl, 2, 5, 0,
    (byte)op_store, 2, 2, 0,
    (byte)op_load, 1, 2, 0,
    (byte)op_swap, 0, 1, 0,
    (byte)op_couti, '\n', 0, 0,
    (byte)op_couti, 'a', 0, 0,
    (byte)op_couti, 'b', 0, 0,
    (byte)op_couti, 'c', 0, 0,
    (byte)op_couti, '\n', 0, 0,
    (byte)op_lcl, 19, 20, 0,
    (byte)op_lcl, 20, 'd', 'e',
    (byte)op_lch, 20, 'f', 'g',
    (byte)op_lcl, 21, 'h', 'i',
    (byte)op_lch, 21, '\n', 0,
    (byte)op_cout, 19, 0, 0,
    (byte)op_halt, 0, 0, 0
};

byte fact[] = {
    (byte)op_lcl, 1, 11, 0, //n
    (byte)op_lcl, 2, 9, 0, //end
    (byte)op_lcl, 3, 4, 0, //loop
    (byte)op_lcl, 4, 1, 0, //val
    (byte)op_gti, 5, 1, 1, //test
    (byte)op_jez, 6, 5, 2, //ra
    (byte)op_mul, 4, 4, 1,
    (byte)op_subi, 1, 1, 1,
    (byte)op_j, 6, 3, 0,
    (byte)op_halt, 0, 0, 0
};

const size_t STACK_SIZE = 512*1024;
word stackMem[STACK_SIZE];

byte* stack = (byte*)stackMem;
byte* stackBase = stack+STACK_SIZE;

int main() {
    //    *stackBase = 32;
    //    cout << "yeah " << (size_t)stack << endl;
    //    setWord(stack, 0, (size_t)stack+STACK_SIZE);
    //    run(stack, STACK_SIZE, (Instruction*)test);
    run(stack, STACK_SIZE, (Instruction*)fact);
    cout << "final " << getWord(stack, 4) << endl;
    printMem(stackMem, 64);
    return 0;
}
