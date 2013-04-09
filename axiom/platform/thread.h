#ifndef axiom_platform_thread_H_
#define axiom_platform_thread_H_

#ifdef PLATFORM_POSIX
#include <pthread.h>
#elif defined(PLATFORM_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#error "axiom_platform_thread: unsupported platform"
#endif

namespace platform {
    namespace thread {

#ifdef PLATFORM_POSIX
        typedef pthread_mutex_t Mutex;
        typedef pthread_cond_t Condition;
        typedef pthread_key_t LocalVar;
        typedef pthread_t ThreadId;
#define ThreadFuncCC
#elif defined(PLATFORM_WIN32)
        typedef CRITICAL_SECTION Mutex;
        typedef HANDLE Condition;
        typedef DWORD LocalVar;
        typedef DWORD ThreadId;
#define ThreadFuncCC __stdcall
#endif
        typedef void*(ThreadFuncCC *ThreadFunc)(void*);

        void makeMutex(Mutex&);
        void freeMutex(Mutex&);
        void lockMutex(Mutex&);
        bool tryLockMutex(Mutex&);
        void unlockMutex(Mutex&);

        struct ScopedMutex {
            ScopedMutex() { makeMutex(mutex_); }
            ~ScopedMutex() { freeMutex(mutex_); }
            void lock() { lockMutex(mutex_); }
            void unlock() { unlockMutex(mutex_); }
        private:
            // non-copyable
            ScopedMutex(const ScopedMutex&);
            void operator=(const ScopedMutex&);
            friend struct ScopedLock;
            friend struct ScopedCondition;
            Mutex mutex_;
        };

        struct ScopedLock {
            ScopedLock(ScopedMutex& m) : mutex_(m.mutex_) { lockMutex(mutex_); }
            ScopedLock(Mutex& m) : mutex_(m) { lockMutex(mutex_); }
            ~ScopedLock() { unlockMutex(mutex_); }
        private:
            // non-copyable
            ScopedLock(const ScopedLock&);
            void operator=(const ScopedLock&);
            Mutex& mutex_;
        };

        void makeCondition(Condition&);
        void freeCondition(Condition&);
        void waitCondition(Condition&, Mutex&);
        // todo: timed wait?
        void notifyOneCondition(Condition&);
        void notifyAllCondition(Condition&);

        struct ScopedCondition {
            ScopedCondition() { makeCondition(cond_); }
            ~ScopedCondition() { freeCondition(cond_); }
            void wait(ScopedMutex& m) { waitCondition(cond_, m.mutex_); }
            void notifyOne() { notifyOneCondition(cond_); }
            void notifyAll() { notifyAllCondition(cond_); }
        private:
            // non-copyable
            ScopedCondition(const ScopedCondition&);
            void operator=(const ScopedCondition&);
            Condition cond_;
        };

        void makeLocalVar(LocalVar&);
        void freeLocalVar(const LocalVar&);
        void setLocalVar(const LocalVar&, void*);
        void* getLocalVar(const LocalVar&);

        struct ScopedLocalVar {
            ScopedLocalVar() { makeLocalVar(key_); }
            ~ScopedLocalVar() { freeLocalVar(key_); }
            // really const?
            void set(void* value) const { setLocalVar(key_, value); }
            void* get() const { return getLocalVar(key_); }
        private:
            // non-copyable
            ScopedLocalVar(const ScopedLocalVar&);
            void operator=(const ScopedLocalVar&);
            LocalVar key_;
        };

        ThreadId getId();
        void terminate(void* retval=0);
        void yield();

        // use a C-style interface to avoid needing to use exceptions
        void start(ThreadId& id, ThreadFunc func, void* arg);
        void start(ThreadFunc func, void* arg);
        void detach(const ThreadId& id);
        void join(const ThreadId& id, void** retval=0);
    }
}

#endif
