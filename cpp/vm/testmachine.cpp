#include "machine.h"
#include "ops.h"
#include <iostream>
using namespace std;

byte sum[] = {
    op_lcl, 1, 0, 0, // result
    op_lcl, 2, 10, 0, // n
    op_bez_i, 0, 2, 3, // if i != 0 (+2)
    op_add, 1, 1, 2, // result += n
    op_sub_i, 2, 2, 1, // --n
    op_b_i, 0, 0xff-3, 0xff, // end if (-4)
    op_halt, 0, 0, 0
};

byte factorial[] = {
    op_lcl, 1, 7, 0, //n
    op_lcl, 2, 9, 0, //end
    op_lcl, 3, 4, 0, //loop
    op_lcl, 4, 1, 0, //result
    op_gt_i, 5, 1, 1, //test
    op_jez, 6, 5, 2, //ra
    op_mul, 4, 4, 1,
    op_sub_i, 1, 1, 1,
    op_j, 6, 3, 0,
    op_halt, 0, 0, 0
};

byte factorial_rec[] = {
    op_lcl, 1, 1, 0, // result
    op_lcl, 2, 7, 0, // n
    op_lcl, 4, 15, 0, // ra = halt
    op_alloc_i, 3, 128, 0, // sp = alloc(128)
    op_add_i, 3, 3, 128, // sp += 128
    op_gte_i, 0, 2, 2, // test = n >= 2
    op_jez, 0, 0, 4, // if !test (return result)
    op_mul, 1, 1, 2, // result *= n
    op_sub_i, 2, 2, 1, // --n
    op_sub_i, 3, 3, 4, // --sp
    op_store, 4, 3, 0, // push ra
    op_b_i, 4, -7, -1,//0xff-6, 0xff, // call again
    op_load, 4, 3, 0, // pop ra
    op_add_i, 3, 3, 4, // ++sp
    op_j, 0, 4, 0, // return
    op_print, 0, 1, 0,
    //    op_lcl, 10, '\n', 0,
    //    op_sub_i, 3, 3, 4,
    //    op_store, 10, 3, 0,
    //    op_add_i, 10, 3, 
    //    op_putch, 0, 3, 0,
    op_putch_i, 0, '\n', 0,
    op_halt, 0, 0, 0
};

byte arith_test[] = {
    op_lcl, 0, 0, 0,
    op_lch, 0, 0, 0,
    op_lcl, 1, 0, 0,
    op_lch, 1, 0, 0,
    op_lcl, 2, 2, 0,
    op_lch, 2, 0, 1,
    op_lcl, 3, 1, 0,
    op_lch, 3, 0, 1,
    op_muld, 0, 2, 3, // mega-mult
    op_halt, 0, 0, 0
};

int main() {
    prettyProg(cout, (const word*)arith_test,
               sizeof(arith_test)/sizeof(word));
    State s;
    run(s, (word*)arith_test);
//     run(s, (word*)sum);
//     run(s, (word*)factorial);
    s.view(cout);
    // seeing if op_muld actually works
    word a = s.wordRegs[2];
    word b = s.wordRegs[3];
    cout << a << endl;
    cout << b << endl;
    cout << *((dword*)(&s.wordRegs[0])) << endl;
    dword result = (dword)a * b;
    cout << result << endl;
    return 0;
}
