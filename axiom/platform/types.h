#ifndef axiom_platform_types_H_
#define axiom_platform_types_H_

namespace platform {
    typedef unsigned char Byte;
    typedef unsigned short HalfWord;
    typedef unsigned Word;
    //    typedef unsigned long long DoubleWord;
    typedef Byte* BytePtr;
    typedef Word* WordPtr;

    union PackedDouble {
        double d;
        struct { Word hi, low; };
    };

    inline void writeDouble(Word* dst, double d) {
        PackedDouble pd;
        pd.d = d;
        dst[0] = pd.hi;
        dst[1] = pd.low;
    }

    inline double readDouble(const Word* src) {
        PackedDouble pd;
        pd.hi = src[0];
        pd.low = src[1];
        return pd.d;
    }
}

#endif
