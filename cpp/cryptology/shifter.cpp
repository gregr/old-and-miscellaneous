// shifter

#include <iostream>
#include <string>
using namespace std;

inline int Resolve(int ch) {
	return (((ch - 'A') % 26) + 'A');
}

int main(int argc, char** argv) {
	assert(argc >= 2);
	int shift = atoi(argv[1]);
	int ch = cin.get();
	while (!cin.eof()) {
		cout.put(Resolve(toupper(ch)+shift));
		ch = cin.get();
	}
	return 0;
}
