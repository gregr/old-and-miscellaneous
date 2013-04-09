#ifndef axiom_platform_msg_H_
#define axiom_platform_msg_H_

#ifndef NDEBUG
#define ASSERT(b) if (b); else fail(__FILE__, __LINE__)
#define ASSERT_MSG(b, msg) if (b); else fail(__FILE__, __LINE__, msg)
#define ASSERT_MSGDETAILED(b, m, d) if (b); else fail(__FILE__, __LINE__, m, d)
#else
#define ASSERT(b)
#define ASSERT_MSG(b, msg)
#define ASSERT_MSGDETAILED(b, m, d)
#endif

namespace platform {
    void error(const char*);
    void info(const char*);
    inline void debug(const char* msg) {
#ifndef NDEBUG
        info(msg);
#endif
    }

    void halt(int);
    void fail(const char* file, unsigned line,
              const char* msg="fail()", const char* msgDetailed="");
    void syserr(const char*);
}

#endif
