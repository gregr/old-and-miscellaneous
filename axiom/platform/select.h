#ifndef axiom_platform_select_H_
#define axiom_platform_select_H_

#ifdef PLATFORM_POSIX
//todo: FD_SETSIZE?
//The default value of FD_SETSIZE (currently 256) is smaller than the default limit on the number of open files. To accommodate programs that may use a larger number of open files with select(), it is possible to increase this size within a program by providing a larger definition of FD_SETSIZE before the inclusion of <sys/select.h>.
#include <sys/select.h>
#elif defined(PLATFORM_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#endif
//#include <stdio.h>

namespace platform {

    struct FdSet {
        FdSet() { clear(); }
        void clear();
        bool has(int fd) const;
        void add(int fd);
        void remove(int fd);
        int end() const { return end_; }
        fd_set* get() { return &fds_; }
    private:
        fd_set fds_;
        int end_;
    };

    struct FdSetIter {
        FdSetIter(const FdSet& fds) : fds_(fds), fdpos_(-1) { next(); }
        int operator++() { next(); return fdpos_; }
        int operator*() const { return fdpos_; }
    private:
        void operator=(const FdSetIter&); // fds_ prevents assignment
        void next() {
            do {
                if (fdpos_ == fds_.end())
                    break;
                ++fdpos_;
            } while (!fds_.has(fdpos_));
        }
        const FdSet& fds_;
        int fdpos_;
    };

    int select(FdSet* readfds=0, FdSet* writefds=0, FdSet* exceptFds=0,
               bool timeout=false, int secTimeout=0, int usecTimeout=0);
}

#endif
