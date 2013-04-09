#include "stringtable.h"

void StringTable_init(StringTable* st) { ARRAY_init(st->array); }

void StringTable_destroy(StringTable* st) {
    ARRAY_foreach(char*, st->array, free(*x));
    ARRAY_destroy(st->array);
}

const char* StringTable_get(const StringTable* st, const char* s) {
    ARRAY_foreach(char*, st->array, {if (strcmp(*x, s) == 0) return *x;});
    return NULL;
}

const char* StringTable_add(StringTable* st, const char* s) {
    const char* s_ = StringTable_get(st, s);
    if (s_ !=NULL ) return s_;
    else {
        ARRAY_addOne(char*, st->array);
        char** sp = ARRAY_top(st->array);
        MALLOC(*sp, strlen(s)+1);
        strcpy(*sp, s);
        return *sp;
    }
}
