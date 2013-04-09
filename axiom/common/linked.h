#ifndef axiom_common_list_H_
#define axiom_common_list_H_

#define SLIST_PUSH(root, x) do {                \
        (x)->next = (root);                     \
        (root) = x;                             \
    } while (0)

#define SLIST_POP(lhs, root) do {               \
        lhs = (root);                           \
        (root) = (root)->next;                  \
    } while (0)

#define DLIST_PUSH(root, x) do {                \
        if ((root) != 0) (root)->prev = x;      \
        (x)->next = (root);                     \
        (x)->prev = 0;                          \
        (root) = x;                             \
    } while (0)

#define DLIST_REMOVE_PREVNAME(root, x, prevName) do {                   \
        if ((x)->next != 0) (x)->next->prevName = (x)->prevName;        \
        if ((x)->prevName != 0) (x)->prevName->next = (x)->next;        \
        else root = (x)->next;                                          \
    } while (0)

#define DLIST_REMOVE(root, x) DLIST_REMOVE_PREVNAME(root, x, prev)

#define DLIST_POPPUSH_PREVNAME(root, x, y, prevName) do {       \
        y->prevName = 0;                                        \
        y->next = x->next;                                      \
        if (x->next != 0) x->next->prevName = y;                \
        root = y;                                               \
    } while (0)

#define DLIST_POPPUSH(root, x, y) DLIST_POPPUSH_PREVNAME(root, x, y, prev)

#define LIST_FOREACH(ty, root, action) do {     \
        ty *next, *x = root;                    \
        while (x != 0) {                        \
            next = x->next;                     \
            action;                             \
            x = next;                           \
        }                                       \
    } while (0)

#endif
