#include "thread.h"
#include "error.h"
#ifdef PLATFORM_WIN32
#include <process.h>
#endif

namespace platform {
    namespace thread {

#ifdef PLATFORM_WIN32
        template <typename ResultType> // support handles as well as ints
        static void check(ResultType result) { if (result == 0) sysErr(); }

        static const unsigned csSpinCount = 4000; // todo: profile

        void makeMutex(Mutex& m) {
            check(InitializeCriticalSectionAndSpinCount(&m, csSpinCount));
        }

        void freeMutex(Mutex& m) { DeleteCriticalSection(&m); }

        void lockMutex(Mutex& m) { EnterCriticalSection(&m); }

        bool tryLockMutex(Mutex& m) { return TryEnterCriticalSection(&m) != 0; }

        void unlockMutex(Mutex& m) { LeaveCriticalSection(&m); }

        // would be nice to use real condition variables, but they're not even
        // supported on windows xp (see msdn), so use events for now
        void makeCondition(Condition& c) {
            c = CreateEvent(0, FALSE, FALSE, 0);
            check(c);
        }

        void freeCondition(Condition& c) { check(CloseHandle(c)); }

        void waitCondition(Condition& c, Mutex& m) {
            // todo: is there a race here?
            unlockMutex(m);
            if (WaitForSingleObject(c, INFINITE) == WAIT_FAILED)
                sysErr();
            lockMutex(m);
        }

        void notifyOneCondition(Condition& c) { check(SetEvent(c)); }

        // msdn says this is unreliable (see above re: condition variables)
        void notifyAllCondition(Condition& c) { check(PulseEvent(c)); }

        void makeLocalVar(LocalVar& k) {
            k = TlsAlloc();
            if (k == TLS_OUT_OF_INDEXES) // msdn says -1?
                sysErr();
        }

        void freeLocalVar(const LocalVar& k) { check(TlsFree(k)); }
        void setLocalVar(const LocalVar& k, void* v) {
            check(TlsSetValue(k, v));
        }

        void* getLocalVar(const LocalVar& k) {
            void* v = TlsGetValue(k);
            if (GetLastError() != NO_ERROR)
                sysErr();
            return v;
        }

        ThreadId getId() { return GetCurrentThreadId(); }
        void terminate(void* retval) { _endthreadex((unsigned)retval); }
        void yield() { Sleep(0); }

        void start(ThreadId& id, ThreadFunc func, void* arg) {
            // msdn says this uses errno on win32, not GetLastError...
            if(_beginthreadex(0, 0,(unsigned (ThreadFuncCC *)(void*))func,
                              arg, 0, (unsigned*)&id) == 0)
                sysErr(errno);
        }

        void detach(const ThreadId&) {} // do nothing on win32?

        void join(const ThreadId& id, void** retval) {
            HANDLE h = OpenThread(SYNCHRONIZE, FALSE, id);
            if (h == 0) sysErr();
            if (WaitForSingleObject(h, INFINITE) == WAIT_FAILED)
                sysErr();
            if (retval != 0) {
                h = OpenThread(THREAD_QUERY_INFORMATION, FALSE, id);
                if (h == 0) sysErr();
                DWORD result;
                if (GetExitCodeThread(h, &result) == 0)
                    sysErr();
                *retval = (void*)result;
            }
        }
#endif

#ifdef PLATFORM_POSIX
        static void check(int result) { if (result != 0) sysErr(); }

        // todo: mutex attrs for debug?
        void makeMutex(Mutex& m) { check(pthread_mutex_init(&m, 0)); }
        void freeMutex(Mutex& m) { check(pthread_mutex_destroy(&m)); }
        // todo: debug-only error checking
        void lockMutex(Mutex& m) {
            pthread_mutex_lock(&m);
        }

        bool tryLockMutex(Mutex& m) { return pthread_mutex_trylock(&m) == 0; }

        // todo: debug-only error checking
        void unlockMutex(Mutex& m) {
            pthread_mutex_unlock(&m); 
        }

        // todo: cond attrs?
        void makeCondition(Condition& c) { check(pthread_cond_init(&c, 0)); }
        void freeCondition(Condition& c) { check(pthread_cond_destroy(&c)); }

        // todo: debug error check
        void waitCondition(Condition& c, Mutex& m) {
            pthread_cond_wait(&c, &m);
        }

        // todo: debug error check
        void notifyOneCondition(Condition& c) {
            pthread_cond_signal(&c);
        }

        // todo: debug error check
        void notifyAllCondition(Condition& c) {
            pthread_cond_broadcast(&c);
        }

        void makeLocalVar(LocalVar& k) { check(pthread_key_create(&k, 0)); }

        // todo: debug-only error checking? destructors shouldn't throw...
        void freeLocalVar(const LocalVar& k) {
            if (pthread_key_delete(k) != 0)
                sysErr();
        }

        void setLocalVar(const LocalVar& k, void* v) {
            check(pthread_setspecific(k, v));
        }

        void* getLocalVar(const LocalVar& k) {
            return pthread_getspecific(k);
        }

        ThreadId getId() {
            return pthread_self();
        }

        void terminate(void* retval) {
            pthread_exit(retval);
        }

        // todo: debug error check? (no errnos defined)
        void yield() { sched_yield(); }

        void start(ThreadId& id, ThreadFunc func, void* arg) {
            check(pthread_create(&id, 0, func, arg));
        }

        void detach(const ThreadId& id) { check(pthread_detach(id)); }
        void join(const ThreadId& id, void** retval) {
            check(pthread_join(id, retval));
        }
#endif

        void start(ThreadFunc func, void* arg) {
            ThreadId id;
            start(id, func, arg);
            detach(id);
        }
    }
}
