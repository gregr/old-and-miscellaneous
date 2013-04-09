#include "machine.h"
#include "ops.h"
#include <iostream>
using namespace std;

void prettyProg(ostream& out, const word* prog, unsigned len) {
    const word* end = prog+len;
    for (; prog != end; ++prog) {
        const byte* cur = reinterpret_cast<const byte*>(prog);
        out << opNames[cur[0]] << ' '
            << (word)cur[1] << ' '
            << (word)cur[2] << ' '
            << (word)cur[3] << endl;
    }
}

template <typename T, typename M>
T getMem(const M* mem) {
    return *(reinterpret_cast<const T*>(mem));
}

template <typename T, typename L, typename R>
void assign(L* lhs, unsigned offset, const R* rhs) {
    *(reinterpret_cast<T*>(lhs)+offset) = getMem<T>(rhs);
}

inline void jump(const word* iBase, const word** ip, word* ret, word dst) {
//     cout << "jumping: " << dst << endl;
//     sleep(1);
    *ret = *ip-iBase;
    *ip = iBase + dst;
}

inline void branch(const word* iBase, const word** ip, word* ret, signed off) {
//     cout << "branching: " << off << endl;
//     sleep(1);
    *ret = *ip-iBase;
    *ip = *ip+off;
}

word allocate(word nBytes) {
    word result = reinterpret_cast<word>(malloc(nBytes));
    //    cout << "allocated " << nBytes << " at: " << result << endl;
    return result;
}

void run(State& state, const vector<word>& instrs) {
    run(state, &instrs[0]);
}

void run(State& state, const word* instrs) {
    word* regs = &state.wordRegs[0];
    double* fregs = &state.floatRegs[0];
    const word* ip = instrs;
    for (;;) {
        const byte* cur = (const byte*)ip;
        ++ip;
        switch ((Op)cur[0]) {
            case op_load:
                regs[cur[1]] = *((const word*)(regs[cur[2]]) + cur[3]);
                break;
            case op_loadb:
                regs[cur[1]] = *((const byte*)(regs[cur[2]]) + cur[3]);
                break;
            case op_loadf:
                fregs[cur[1]] = *((const double*)(regs[cur[2]]) + cur[3]);
                break;
            case op_store:
                *((word*)(regs[cur[2]]) + cur[3]) = regs[cur[1]];
                break;
            case op_storeb:
                *((byte*)(regs[cur[2]]) + cur[3]) = (byte)regs[cur[1]];
                break;
            case op_storef:
                *((double*)(regs[cur[2]]) + cur[3]) = fregs[cur[1]];
                break;
            case op_lch: // assuming little-endian
                assign<byte2>(regs+cur[1], 1, cur+2);
                break;
            case op_lcl:
                assign<byte2>(regs+cur[1], 0, cur+2);
                break;
            case op_lcf:
                fregs[cur[1]] = (double)getMem<short>(cur+2);
                break;
            case op_shl:
                regs[cur[1]] = regs[cur[2]] << regs[cur[3]];
                break;
            case op_shl_i:
                regs[cur[1]] = regs[cur[2]] << cur[3];
                break;
            case op_shr:
                regs[cur[1]] = regs[cur[2]] >> regs[cur[3]];
                break;
            case op_shr_i:
                regs[cur[1]] = regs[cur[2]] >> cur[3];
                break;
            case op_shrs:
                regs[cur[1]] = (unsigned)((signed)regs[cur[2]] >> regs[cur[3]]);
                break;
            case op_shrs_i:
                regs[cur[1]] = (unsigned)((signed)regs[cur[2]] >> cur[3]);
                break;
            case op_add:
                regs[cur[1]] = regs[cur[2]] + regs[cur[3]];
                break;
            case op_add_i:
                //                cout << "add " << regs[cur[1]] << " " << regs[cur[2]] + cur[3] << endl;
                regs[cur[1]] = regs[cur[2]] + cur[3];
                break;
            case op_sub:
                regs[cur[1]] = regs[cur[2]] - regs[cur[3]];
                break;
            case op_sub_i:
                //                cout << "sub " << regs[cur[1]] << " " << regs[cur[2]] - cur[3] << endl;
                regs[cur[1]] = regs[cur[2]] - cur[3];
                break;
            case op_mul:
                regs[cur[1]] = regs[cur[2]] * regs[cur[3]];
                break;
            case op_mul_i:
                regs[cur[1]] = regs[cur[2]] * cur[3];
                break;
            case op_muld: // must ensure that regs+cur[1] is properly aligned
                *((dword*)(regs+cur[1])) = (dword)regs[cur[2]] * regs[cur[3]];
                break;
            case op_muld_i: // ditto
                *((dword*)(regs+cur[1])) = (dword)regs[cur[2]] * cur[3];
                break;
            case op_div:
                regs[cur[1]] = regs[cur[2]] / regs[cur[3]];
                break;
            case op_div_i:
                regs[cur[1]] = regs[cur[2]] / cur[3];
                break;
            case op_mod:
                regs[cur[1]] = regs[cur[2]] % regs[cur[3]];
                break;
            case op_mod_i:
                regs[cur[1]] = regs[cur[2]] % cur[3];
                break;
            case op_eq:
                regs[cur[1]] = regs[cur[2]] == regs[cur[3]];
                break;
            case op_eq_i:
                regs[cur[1]] = regs[cur[2]] == cur[3];
                break;
            case op_gte:
                regs[cur[1]] = regs[cur[2]] >= regs[cur[3]];
                break;
            case op_gte_i:
                regs[cur[1]] = regs[cur[2]] >= cur[3];
                break;
            case op_gt:
                regs[cur[1]] = regs[cur[2]] > regs[cur[3]];
                break;
            case op_gt_i:
                regs[cur[1]] = regs[cur[2]] > cur[3];
                break;
            case op_and:
                regs[cur[1]] = regs[cur[2]] & regs[cur[3]];
                break;
            case op_or:
                regs[cur[1]] = regs[cur[2]] | regs[cur[3]];
                break;
            case op_nor: // masochists can use this (or nand) for everything
                regs[cur[1]] = ~(regs[cur[2]] | regs[cur[3]]);
                break;
            case op_xor:
                regs[cur[1]] = regs[cur[2]] ^ regs[cur[3]];
                break;
            case op_j:
                jump(instrs, &ip, regs+cur[1], regs[cur[2]]);
                break;
            case op_jez:
                if (regs[cur[2]] == 0)
                    jump(instrs, &ip, regs+cur[1], regs[cur[3]]);
                break;
            case op_jnz:
                if (regs[cur[2]] != 0)
                    jump(instrs, &ip, regs+cur[1], regs[cur[3]]);
                break;
            case op_b:
                branch(instrs, &ip, regs+cur[1], (signed)regs[cur[2]]);
                break;
            case op_b_i:
                branch(instrs, &ip, regs+cur[1], getMem<sbyte2>(cur+2));
                break;
            case op_bez:
                if (regs[cur[2]] == 0)
                    branch(instrs, &ip, regs+cur[1], (signed)regs[cur[3]]);
                break;
            case op_bez_i:
                if (regs[cur[2]] == 0)
                    branch(instrs, &ip, regs+cur[1], (sbyte)cur[3]);
                break;
            case op_bnz:
                if (regs[cur[2]] != 0)
                    branch(instrs, &ip, regs+cur[1], (signed)regs[cur[3]]);
                break;
            case op_bnz_i:
                if (regs[cur[2]] != 0)
                    branch(instrs, &ip, regs+cur[1], (sbyte)cur[3]);
                break;
            case op_stof:
                fregs[cur[1]] = (double)(signed)regs[cur[2]];
                break;
            case op_utof:
                fregs[cur[1]] = (double)regs[cur[2]];
                break;
            case op_ftos:
                regs[cur[1]] = (signed)fregs[cur[2]];
                break;
            case op_ftou:
                regs[cur[1]] = (word)fregs[cur[2]];
                break;
                // todo: op_dealloc! duh
            case op_alloc:
                regs[cur[1]] = allocate(regs[cur[2]]);
                break;
            case op_alloc_i:
                regs[cur[1]] = allocate(getMem<byte2>(cur+2));
                break;
            case op_print: // for testing
                // regs[cur[1]] // channel unused for now
                cout << (int)regs[cur[2]] << flush;
                break;
            case op_putch: // should ultimately be given a channel (file)
                // regs[cur[1]] // channel unused for now
                cout << (char)(*((const byte*)regs[cur[2]]));
                break;
            case op_putch_i:
                // regs[cur[1]] // channel unused for now
                cout << (char)cur[2];
                break;
            case op_getch: // see above
                // regs[cur[1]] // channel unused for now
                break;
            case op_nop:
                break;
            case op_halt:
                return;
            default:
                cerr << "Error: Unknown op at: " << (ip-1)-instrs << endl;
                return;
        }
    }
}
