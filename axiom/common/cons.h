#ifndef axiom_cons_H_
#define axiom_cons_H_

#define CONS_STRUCT(ty, structName)                                     \
    typedef struct structName##_{                                       \
        ty head; struct structName##_* tail;                            \
    } structName

#define CONS_FOREACH(ty, head, action) do {                     \
        for (ty* x = head; x != 0; x = x->tail) { action; }       \
    } while (0)

#endif
