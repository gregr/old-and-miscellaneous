// swapper

#include <iostream>
#include <string>
using namespace std;

inline int Resolve(int ch) {
	return (((ch - 'A') % 26) + 'A');
}

int main(int argc, char** argv) {
	assert(argc >= 3);
	int val1 = toupper(*argv[1]);
	int val2 = toupper(*argv[2]);
	int ch = toupper(cin.get());
	string buffer;
	while (!cin.eof()) {
		if (ch == val1)
			ch = val2;
		else if (ch == val2)
			ch = val1;
		buffer += ch;
		ch = toupper(cin.get());
	}
	cout << buffer;
	return 0;
}
