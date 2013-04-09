#include "token.h"
#include <iostream>
using namespace irp;
using namespace std;

int main() {
//     string s = "blah";
//     string b = "bloop";
//     Trie<string> t;
//     t.add(s, s);
//     t.add(b, b);
//     cout << "enter key: " << flush;
//     string* v = t.find(cin);
//     if (v == 0) cout << "unknown key" << endl;
//     else cout << *v << endl;
    try {
        TokenContext ctx;
        while (cin) {
            showToken(cout, getToken(ctx, cin));
            cout << endl;
        }
    } catch (const exception& e) { cerr << e.what() << endl; }
    //    catch (...) { cerr << "caught unknown exception" << endl; }
    return 0;
}
