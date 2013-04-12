#ifndef axiom_platform_error_H_
#define axiom_platform_error_H_

#include <stdexcept>

namespace platform {

    struct SystemError : public std::runtime_error {
        SystemError(const char* msg, int code_=0)
            : std::runtime_error(msg), code(code_) {}
        int code;
    };

    int lastErr();
    const char* lastErrStr(int code);
    void sysErr();
    void sysErr(int code);
    void sysErr(const char* prefix);
    void sysErr(const char* prefix, int code);
}

#endif
