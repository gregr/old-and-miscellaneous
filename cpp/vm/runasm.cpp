#include "assembler.h"
//#include "machine.h"
#include <fstream>
#include <iostream>
#include <stdexcept>
using namespace std;

// TODO: parse options like -v for verbose output (pretty-print program?)
// and something to display registers
// plus some options for dealing with binary code files
// FUTURE: when reading a file name "prog.asm" that already has a "prog.code"
// file written, do some kind of checksum or modification time check to see if
// we can simply run the bytecode file without having to reassemble
int main(int argc, const char** argv) {
    try {
        if (argc < 2)
            cout << "usage: runasm FILENAME" << endl;
        else {
            ifstream fin(argv[1], ios_base::in|ios_base::binary);
            vector<word> code = assembleProg(fin);
            // todo: serialize code to file?
            State s;
            run(s, code);
            s.view(cout); // for debugging until a new flag is added
        }
    } catch (const exception& e) {
        cerr << "exception: " << e.what() << endl;
    } catch (...) {
        cerr << "unknown error: " << endl;
    }
    return 0;
}
