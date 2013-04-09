#ifndef axiom_common_testing_H_
#define axiom_common_testing_H_

#include <stdio.h>

#ifndef TEST_VERBOSE
#define TEST_VERBOSE 0
#endif

#define TEST_DESC_(cond, ...) do {                                      \
        if (!(cond)) {                                                  \
            result_ = 0;                                                \
            fprintf(stderr, "test failed @ %s:%u in function '%s': %s; ", \
                    __FILE__, __LINE__, __func__, #cond);               \
            fprintf(stderr, __VA_ARGS__);                               \
            fprintf(stderr, "\n");                                      \
        }                                                               \
        else if (TEST_VERBOSE)                                          \
            fprintf(stdout, "test passed @ %s:%u in function '%s': %s\n", \
                    __FILE__, __LINE__, __func__, #cond);               \
    } while (0)

#define TEST_(cond) TEST_DESC_(cond, "no description")

#define TEST_CMP_FMT_(a, cmp, b, fmt) TEST_DESC_((a) cmp (b), #a"="fmt  \
                                                 ", "#b"="fmt, (a), (b))
#define TEST_CMPD_(a, cmp, b) TEST_CMP_FMT_(a, cmp, b, "%d")
#define TEST_CMPU_(a, cmp, b) TEST_CMP_FMT_(a, cmp, b, "%u")
#define TEST_CMPX_(a, cmp, b) TEST_CMP_FMT_(a, cmp, b, "%x")
#define TEST_CMPP_(a, cmp, b) TEST_CMP_FMT_((void*)(a), cmp, (void*)(b), "%p")

#endif
