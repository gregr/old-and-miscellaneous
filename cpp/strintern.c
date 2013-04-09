#include "strintern2.h"
#include <stdio.h>

const char* a = "yes";
const char* b = "yes";

int main() {
    printf("%d, %d, %d\n", a==b, a=="yes", a==extStr);
    return 0;
}
