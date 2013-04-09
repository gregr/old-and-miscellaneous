#ifndef axiom_common_error_H_
#define axiom_common_error_H_

#include <setjmp.h>
#include <stddef.h>

// requires inclusion of stdio.h
#define LOG_ERR(...) fprintf(stderr, __VA_ARGS__)

////////////////////////////////////////////////////////////////
// Exceptions
#define TRYFINALLY(code, fnlly) TRY_BODY_(code, {} {fnlly})
#define TRYCATCH(code, catches) TRY_BODY_(code, {catches {}})
// intentionally test exception_type() with pointer equality
#define CATCH(etype, code) if (exception_type() == etype) {     \
        exception_unsetPending_(); code; } else
#define CATCHALL(code) if (1) { exception_unsetPending_(); code; } else

#define RAISE(type, ...) do {                    \
        EXC_SETSITE_();                          \
        exception_setDesc_(__VA_ARGS__);         \
        raise_(type);                            \
    } while (0)

void reraise(void);

const char* exception_type(void);
int exception_pending(void);
const char* exception_desc(void);
const char* exception_site(void);

// requires inclusion of stdio.h
#define PRINT_EXC(out) fprintf(out, "type='%s', desc='%s', site='%s'",  \
                               exception_type(), exception_desc(),      \
                               exception_site())

////////////////////////////////////////////////////////////////
// Assertions
extern const char* const ET_ASSERT;
#ifdef NDEBUG
#define ASSERT_DESC(cond, ...)
#else
#define ASSERT_DESC(cond, ...) do {                                     \
        if ((cond) == 0) RAISE(ET_ASSERT, __VA_ARGS__); } while (0)
#endif
#define ASSERT(cond) ASSERT_DESC(cond, #cond)

////////////////////////////////////////////////////////////////
// Failure handling
typedef void(*FailHook)(const char*);
FailHook getFailHook(void);
void setFailHook(FailHook);

void fail(const char*);

////////////////////////////////////////////////////////////////
// internal use only
typedef struct TryFrame_ {
    struct TryFrame_* prev;
    jmp_buf buf;
} TryFrame;

void raise_(const char* type);
int exception_setDesc_(const char* fmt, ...);
void exception_setSite_(const char* file, unsigned line, const char* func);
#define EXC_SETSITE_() exception_setSite_(__FILE__, __LINE__, __func__)

void exception_unsetPending_(void);
jmp_buf* exception_pushTryFrame_(TryFrame*);
void exception_popTryFrame_(void);
#define BEGIN_TRY_(code)                                        \
    exception_unsetPending_(); TryFrame frame;                  \
    if (setjmp(*exception_pushTryFrame_(&frame)) == 0) {        \
        {code} exception_popTryFrame_(); } else
#define TRY_BODY_(code, handling) {         \
        BEGIN_TRY_(code) handling if (exception_pending()) reraise(); }

#endif
