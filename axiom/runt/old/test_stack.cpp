#include "impl_stack.h"
#include <iostream>
using namespace runt;
using namespace std;

int main() {
    const size_t stackSize = 1024;
    Stack s;
    newStack(s, stackSize);
    pushFrame(s, 100, 49);
    pushFrame(s, 42, 7);
    showStack(cout, s);
    return 0;
}
