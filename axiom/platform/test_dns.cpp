#include "net.h"
using namespace platform::net;

int showAddrs(const char* host) {
    AddrInfo* info = getAddrInfo(host, 0);
    AddrInfoGuard g(info);
    for (const AddrInfo* p = info; p != 0; p = nextAddrInfo(*p)) {
        printf("%s\n", addrRepr(getAddr(*p)).c_str());
    }
    return 0;
}

int main(int argc, const char** argv) {
    try {
        NetInitGuard guard;
        if (argc == 2) showAddrs(argv[1]);
    } catch (const std::exception& e) {
        printf("caught exception: %s\n", e.what());
    }
    return 0;
}
