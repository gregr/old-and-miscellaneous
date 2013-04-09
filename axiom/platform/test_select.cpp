#include "select.h"
#include "error.h"
#include <stdio.h>
using namespace platform;

void testUnixSelect() { // won't work on win32
    FdSet rfds;
    rfds.add(0);
    printf("enter some text: \n");
    try {
        int count = select(&rfds, 0, 0, true, 3);
        //        int count = select(&rfds);
        // it better have this...
        if (count > 0) {
            if (rfds.has(0))
                printf("got input\n");
        }
        else
            printf("timeout\n");
    } catch (const SystemError& e) {
        printf(e.what());
    }
}

int main() {
    testUnixSelect();
    return 0;
}
