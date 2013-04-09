#include "error.h"
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

// todo: make these thread-local
static void* curFrame_ = 0;
static void* exc_type_ = 0;
static void* exc_pending_ = 0;
#define exc_bufSize 256
static char exc_desc[exc_bufSize];
static char exc_site[exc_bufSize];

const char* exception_type(void) { return exc_type_; }
int exception_pending(void) { return exc_pending_ != 0; }

const char* exception_desc(void) { return exc_desc; }
const char* exception_site(void) { return exc_site; }

int exception_setDesc_(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int result = vsnprintf(exc_desc, exc_bufSize, fmt, args);
    va_end(args);
    return result;
}

void exception_setSite_(const char* file, unsigned line, const char* func) {
    snprintf(exc_site, exc_bufSize, "%s:%u in function '%s'", file, line, func);
}

void raise_(const char* ty) {
    exc_type_ = (void*)ty;
    exc_pending_ = (void*)ty;
    reraise();
}

void reraise(void) {
    if (curFrame_ != 0) {
        jmp_buf* buf = &((TryFrame*)curFrame_)->buf;
        exception_popTryFrame_();
        longjmp(*buf, 1);
    } else fail(exception_type());
}

jmp_buf* exception_pushTryFrame_(TryFrame* f) {
    f->prev = (TryFrame*)curFrame_;
    curFrame_ = f;
    return &((TryFrame*)curFrame_)->buf;
}

void exception_popTryFrame_(void) { curFrame_ = ((TryFrame*)curFrame_)->prev; }
void exception_unsetPending_(void) { exc_pending_ = 0; }

const char* const ET_ASSERT = "assertion failed";

// failure
static FailHook failHook = 0;
FailHook getFailHook() { return failHook; }
void setFailHook(FailHook fh) { failHook = fh; }

void fail(const char* code) {
    if (failHook != 0) (*failHook)(code);
    else {
        if (exception_pending()) {
            LOG_ERR("unhandled exception: ");
            PRINT_EXC(stderr);
            LOG_ERR("\n");
        } else LOG_ERR("unhandled failure: '%s'\n", code);
        exit(1);
    }
}
