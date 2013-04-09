#include "env.h"

void EnvNameTable_init(EnvNameTable* ent) {
    StringTable_init(&ent->syms);
    ARRAY_init(ent->names);
}

void EnvNameTable_destroy(EnvNameTable* ent) {
    StringTable_destroy(&ent->syms);
    ARRAY_foreach(EnvName*, ent->names, free(*x));
    ARRAY_destroy(ent->names);
}

static inline int EnvName_eq(const EnvName* n1, const EnvName* n2) {
    return (strcmp(n1->sym, n2->sym)==0); // && (n1->id == n2->id);
}

const EnvName* EnvNameTable_get(const EnvNameTable* ent, const EnvName* n) {
    ARRAY_foreach(EnvName*, ent->names,
                  {if (EnvName_eq(*x, n)) return *x;});
    return NULL;
}

const EnvName* EnvNameTable_add(EnvNameTable* ent, const EnvName* n) {
    const EnvName* n_ = EnvNameTable_get(ent, n);
    if (n_ != NULL) return n_;
    else {
        const char* sym = StringTable_add(&ent->syms, n->sym);
        ARRAY_addOne(EnvName*, ent->names);
        EnvName** np = ARRAY_top(ent->names);
        MALLOC(*np, sizeof(EnvName));
        (*np)->sym = sym;
        (*np)->id = n->id;
        return *np;
    }
}

void Env_init(Env* e, const Env* parent) {
    e->parent = parent;
    ARRAY_init(e->bindings);
}

void Env_destroy(Env* e) { ARRAY_destroy(e->bindings); }

void Env_destroyData(Env* e, void(*destroy)(void*)) {
    ARRAY_foreach(EnvBinding, e->bindings, { (*destroy)(x->data); });
}

// todo: assert that 'n' is new
void Env_add(Env* e, EnvName* n, void* data) {
    ARRAY_addOne(EnvBinding, e->bindings);
    EnvBinding* b = ARRAY_top(e->bindings);
    b->name = n;
    b->data = data;
}

void* Env_get(const Env* e, const EnvName* n) {
    for (; e != NULL; e = e->parent) {
        ARRAY_foreach(EnvBinding, e->bindings,
                      {if (x->name == n) return x->data;});
    }
    return NULL;
}
