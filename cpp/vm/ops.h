#include <string>

enum Op {
    op_load, op_loadb, op_loadf, op_store, op_storeb, op_storef,
    op_lch, op_lcl, op_lcf,
    op_shl, op_shl_i, op_shr, op_shr_i, op_shrs, op_shrs_i,
    op_add, op_add_i, op_sub, op_sub_i,
    op_mul, op_mul_i, op_muld, op_muld_i,
    op_div, op_div_i, op_mod, op_mod_i,
    op_eq, op_eq_i, op_gte, op_gte_i, op_gt, op_gt_i,
    op_and, op_or, op_nor, op_xor,
    op_j, op_jez, op_jnz,
    op_b, op_b_i, op_bez, op_bez_i, op_bnz, op_bnz_i,
    op_stof, op_utof, op_ftos, op_ftou,
    op_alloc, op_alloc_i,
    op_print, op_putch, op_putch_i, op_getch,
    op_nop, op_halt
};

const char* const opNames[] = {
    "op_load", "op_loadb", "op_loadf", "op_store", "op_storeb", "op_storef",
    "op_lch", "op_lcl", "op_lcf",
    "op_shl", "op_shl_i", "op_shr", "op_shr_i", "op_shrs", "op_shrs_i",
    "op_add", "op_add_i", "op_sub", "op_sub_i",
    "op_mul", "op_mul_i",  "op_muld", "op_muld_i",
    "op_div", "op_div_i", "op_mod", "op_mod_i",
    "op_eq", "op_eq_i", "op_gte", "op_gte_i", "op_gt", "op_gt_i",
    "op_and", "op_or", "op_nor", "op_xor",
    "op_j", "op_jez", "op_jnz",
    "op_b", "op_b_i", "op_bez", "op_bez_i", "op_bnz", "op_bnz_i",
    "op_stof", "op_utof", "op_ftos", "op_ftou",
    "op_alloc", "op_alloc_i",
    "op_print", "op_putch", "op_putch_i", "op_getch",
    "op_nop", "op_halt"
};

const unsigned numOps = sizeof(opNames)/sizeof(opNames[0]);

inline int opByName(const std::string& name) {
    for (int ix = 0; ix != numOps; ++ix)
        if (name == opNames[ix])
            return ix;
    return -1;
}
