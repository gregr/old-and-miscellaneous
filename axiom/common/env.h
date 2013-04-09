#ifndef axiom_common_env_H_
#define axiom_common_env_H_

#include "stringtable.h"

typedef struct {
    const char* sym;
    int id;
} EnvName;

typedef struct {
    StringTable syms;
    ARRAY_STRUCT_BARE(EnvName*) names; // todo: binary search
} EnvNameTable;

void EnvNameTable_init(EnvNameTable*);
void EnvNameTable_destroy(EnvNameTable*);
const EnvName* EnvNameTable_get(const EnvNameTable*, const EnvName*);
const EnvName* EnvNameTable_add(EnvNameTable*, const EnvName*);

typedef struct {
    const EnvName* name;
    void* data;
} EnvBinding;

typedef struct Env_ {
    ARRAY_STRUCT_BARE(EnvBinding) bindings; // todo: binary search
    const struct Env_* parent;
} Env;

void Env_init(Env*, const Env* parent);
void Env_destroy(Env*);
void Env_destroyData(Env*, void(*)(void*));
void Env_add(Env*, EnvName*, void*);
void* Env_get(const Env*, const EnvName*);

#endif
