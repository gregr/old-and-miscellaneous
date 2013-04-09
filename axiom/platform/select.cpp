#include "select.h"
#include "error.h"

namespace platform {

    void FdSet::clear() { FD_ZERO(&fds_); end_ = -1; }

    bool FdSet::has(int fd) const {
        return (fd != -1) && FD_ISSET(fd, &fds_) != 0;
    }

    // todo: wtf is winsock doing inside these FD macros?
// build\select.cpp(13) : warning C4389: '==' : signed/unsigned mismatch
// build\select.cpp(13) : warning C4127: conditional expression is constant
// build\select.cpp(19) : warning C4389: '==' : signed/unsigned mismatch
// build\select.cpp(19) : warning C4127: conditional expression is constant

    void FdSet::add(int fd) {
        FD_SET(fd, &fds_); // (13) msvc complains about this line
        if (fd >= end_)
            end_ = fd+1;
    }

    void FdSet::remove(int fd) {
        FD_CLR(fd, &fds_); // (19) see above complaint
        int last = -2;
        for (int fd = 0; fd != end_; ++fd) {
            if (has(fd))
                last = fd;
        }
        end_ = last+1;
    }

    static int fdsEnd(int current, FdSet* fds) {
        if (fds != 0) {
            int fdsend = fds->end();
            if (fdsend > current)
                return fdsend;
        } 
        return current;
    }

    static fd_set* getFds(FdSet* fds) {
        if (fds == 0)
            return 0;
        return fds->get();
    }

    static timeval* getTimeout(timeval& tv, bool timeout, int secTimeout,
                        int usecTimeout) {
        if (!timeout)
            return 0;
        tv.tv_sec = secTimeout;
        tv.tv_usec = usecTimeout;
        return &tv;
    }

    int select(FdSet* readfds, FdSet* writefds, FdSet* exceptfds,
               bool timeout, int secTimeout, int usecTimeout) {
        int end = fdsEnd(fdsEnd(fdsEnd(-1, readfds), writefds), exceptfds);
        if (end == -1)
            throw SystemError("select: all fd_set arguments were empty", 0);
        timeval tv;
        int result = ::select(end, getFds(readfds), getFds(writefds),
                              getFds(exceptfds),
                              getTimeout(tv, timeout, secTimeout, usecTimeout));
        if (result == -1)
            sysErr("select: ");
        return result;
    }
}
