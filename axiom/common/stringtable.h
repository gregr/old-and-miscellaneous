#ifndef axiom_common_stringtable_H_
#define axiom_common_stringtable_H_

#include "array.h"

typedef struct {
    ARRAY_STRUCT_BARE(char*) array; // todo: binary search?
} StringTable;

void StringTable_init(StringTable*);
void StringTable_destroy(StringTable*);
const char* StringTable_get(const StringTable*, const char*);
const char* StringTable_add(StringTable*, const char*);

#endif
