#include <signal.h>
#include <stdio.h>

void handler(int s) {
    printf("eat shit: %d\n", s);
    //    fflush(stdout);
}

int main(int argc, const char** argv) {
    struct sigaction sa, old;
    //    sa.sa_handler = SIG_IGN;
    sa.sa_handler = handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    int result = sigaction(SIGINT, &sa, &old);
    printf("result: %d\n", result);
    for (;;); // loop forever
    return 0;
}
