#include <iostream>
using namespace std;

int nCalls;

int fib(int n) {
    ++nCalls;
    if (n < 2)
        return n;
    return fib(n-1) + fib(n-2);
}

int main() {
    nCalls = 0;
    for (int n = 0; n < 36; ++n)
        cout << fib(n) << endl;
    cout << endl << nCalls << endl;
}
