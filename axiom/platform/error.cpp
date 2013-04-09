#include "error.h"
#ifdef PLATFORM_POSIX
#include <string.h>
#include <errno.h>
#elif defined(PLATFORM_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif
#include <string>
using namespace std;

namespace platform {

    int lastErr() {
#ifdef PLATFORM_POSIX
        return errno;
#elif defined(PLATFORM_WIN32)
        return GetLastError();
#endif
    }

    // todo: replace strerror on windows? (doesn't seem necessary atm...)
    // warning C4996: 'strerror': This function or variable may be
    // unsafe. Consider using strerror_s instead. To disable deprecation,
    // use _CRT_SECURE_NO_WARNINGS.
    const char* lastErrStr(int code) {
        return strerror(code);
    }

    void sysErr() {
        sysErr("");
    }

    void sysErr(int code) {
        sysErr("", code);
    }

    void sysErr(const char* prefix) {
        sysErr(prefix, lastErr());
    }

    void sysErr(const char* prefix, int code) {
        string msg = prefix;
        msg += lastErrStr(code);
        throw SystemError(msg.c_str(), code);
    }
}
