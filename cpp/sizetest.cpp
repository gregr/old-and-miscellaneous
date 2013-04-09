#include <iostream>
using namespace std;

struct FrameHeader {
    void* p;
    double d;
    size_t s1;
    bool b;
};

struct Thing {
    char a;
    short s;
    char c;
};

union PackedDouble {
    double d;
    struct { unsigned hi, low; };
};

void printPD(const PackedDouble& pd) {
    cout << pd.d << " " << pd.hi << " " << pd.low << endl;
}

void writeDouble(unsigned* dst, double d) {
    PackedDouble pd;
    pd.d = d;
    dst[0] = pd.hi;
    dst[1] = pd.low;
    //    printPD(pd);
}

double readDouble(unsigned* src) {
    PackedDouble pd;
    pd.hi = src[0];
    pd.low = src[1];
    return pd.d;
}

int main() {
    FrameHeader frames[2];
    char bytes[64];
    memset(bytes, 0, sizeof(bytes));
    unsigned words[32];
    cout << sizeof(Thing) << endl;
    cout << sizeof(FrameHeader) << endl;
    cout << sizeof(frames) << endl;
    cout << sizeof(bytes) << endl;
    FrameHeader f;
    //    f.d = 3.14;
    f.p = (void*)54;
    f.b = true;
    size_t index = 1;
    memcpy(bytes+index, &f, sizeof(f));
    FrameHeader* pf = (FrameHeader*)(bytes+index);
    //    pf->d = 3.14;
    bytes[0] = 'h';
    writeDouble(words+7, 3.14);
    cout << readDouble(words+7) << endl;
//     cout << pf->p << endl;
//     cout << pf->b << endl;
//     cout << (int)bytes << endl;
//     cout << (int)(bytes+index) << endl;
    return 0;
}
