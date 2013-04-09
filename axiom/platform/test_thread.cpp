#include "thread.h"
#include <stdio.h>
#ifdef PLATFORM_POSIX
#include <unistd.h> // for sleep
#endif
using namespace platform::thread;

ScopedMutex mutex;

void* ThreadFuncCC threadMain(void* arg) {
    {ScopedLock lock(mutex);}
#ifdef PLATFORM_POSIX // todo: platform-independent sleep
    sleep(1); // seconds... would like to specify milliseconds?
#elif defined(PLATFORM_WIN32)
    Sleep(1000);
#endif
    ScopedLock lock(mutex);
    for (int i = 0; i < 1000; ++i) {
        printf("%s", (const char*)arg);
        yield();
    }
    return 0;
}

void testThreads() {
    ThreadId t1, t2, t3;
    {
        ScopedLock lock(mutex);
        start(t1, threadMain, (void*)"X");
        start(t2, threadMain, (void*)"o");
        start(t3, threadMain, (void*)"A");
    }
    join(t1);
    join(t2);
    join(t3);
    printf("\n");
}

int main() {
    testThreads();
    return 0;
}
