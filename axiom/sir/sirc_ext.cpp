#include <stdio.h>

extern "C" {
void hello() { printf("hello\n"); }
void testPrint(int i) { printf("int: %d\n", i); }
}
