#include "msg.h"
#include <iostream>
#include <sstream>
#include <cstdlib>
using namespace std;

namespace platform {

    void info(const char* msg) {
        cout << msg;
    }

    void error(const char* msg) {
        cerr << msg;
    }

    void halt(int status) {
        exit(status);
    }

    void fail(const char* file, unsigned line,
              const char* msg, const char* msgDetailed) {
        ostringstream ss;
        ss << file << ":" << line << ": " << msg << msgDetailed << endl;
        error(ss.str().c_str());
        halt(EXIT_FAILURE); // abort instead?
    }

    void syserr(const char* msg) { // todo
        error(msg);
    }
}
