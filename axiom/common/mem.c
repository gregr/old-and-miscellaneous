#include <stddef.h>

const char* const ET_MEM = "out of memory";

int(*mem_failHook)(size_t sz) = 0;
