#include "net.h"
#include "error.h"
#ifdef PLATFORM_POSIX
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <errno.h>
#define closesocket close
#define optval void
#elif defined(PLATFORM_WIN32)
#define socklen_t int
#define optval char
#endif
#include <string.h>


// none of these seems necessary at the moment...
//#include <sys/types.h>
//#include <sys/socket.h>
//#include <netinet/in.h>
//#include <sys/wait.h>
//#include <xti.h>
//#include <unistd.h>

namespace platform {
    namespace net {

        static const char* aiLastErrStr(int aiErrCode) {
            return gai_strerror(aiErrCode);
        }

        static void aiErr(int aiErrCode, const char* prefix="") {
            std::string msg = prefix;
            msg += aiLastErrStr(aiErrCode);
            throw SystemError(msg.c_str(), aiErrCode);
        }

        static void aiCheck(int result, const char* prefix="") {
            if (result != 0) aiErr(result, prefix);
        }

        static int check(int result, const char* prefix="") {
            if (result == -1) sysErr(prefix);
            return result;
        }

#ifdef PLATFORM_WIN32
        WSADATA wsaData;
#endif

        void init() {
#ifdef PLATFORM_WIN32
            int result = WSAStartup(MAKEWORD(1, 1), &wsaData);
            // todo: is there a str conversion for this error result?
            if (result != 0)
                throw SystemError("net::init: WSAStartup() failed", result);
#endif
        }

        void quit() {
#ifdef PLATFORM_WIN32
            check(WSACleanup(), "net::quit: ");
#endif
        }

        std::string getHostName() {
            char name[256];
            check(gethostname(name, sizeof(name)));
            return name;
        }

        // todo: why is NI_NUMERICSCOPE not defined anywhere?
        std::string getAddrHost(const Addr& addr) {
            char buffer[INET6_ADDRSTRLEN];
            aiCheck(getnameinfo((const sockaddr*)&addr, sizeof(addr), buffer,
                                sizeof(buffer), 0, 0, NI_NUMERICHOST
                                //                            |NI_NUMERICSCOPE
                                ),
                    "getAddrHost: ");
            return buffer;
        }

        std::string getAddrService(const Addr& addr) {
            char buffer[INET6_ADDRSTRLEN];
            aiCheck(getnameinfo((const sockaddr*)&addr, sizeof(addr), 0, 0,
                                buffer, sizeof(buffer), NI_NUMERICSERV
//                                                            |NI_NUMERICSCOPE
                                ),
                    "getAddrService: ");
            return buffer;
        }

        const AddrInfo* nextAddrInfo(const AddrInfo& info) {
            return info.ai_next;
        }

        const Addr& getAddr(const AddrInfo& info) {
            return *(Addr*)(info.ai_addr);
        }

        AddrInfo* getAddrInfo(const char* host, const char* service,
                              int socketType, int family, int flags) {
            AddrInfo* result;
            AddrInfo hints;
            memset(&hints, 0, sizeof(hints));
            hints.ai_flags = flags;
            hints.ai_family = family;
            hints.ai_socktype = socketType;
            //            hints.ai_protocol = 0;
            aiCheck(getaddrinfo(host, service, &hints, &result),
                    "getAddrInfo: ");
            return result;
        }

        AddrInfo* getLocalAddrInfo(const char* service, int sockType,
                                   int family, int flags) {
            return getAddrInfo(0, service, sockType, family, flags|AI_PASSIVE);
        }

        AddrInfo* getLoopbackAddrInfo(const char* service, int socketType,
                                      int family, int flags) {
            return getAddrInfo(0, service, socketType, family, flags);
        }

        void freeAddrInfo(AddrInfo* info) { freeaddrinfo(info); }

        void getLocalAddr(Socket s, Addr& addr) {
            socklen_t len = sizeof(addr);
            check(getsockname(s, (sockaddr*)&addr, &len), "getLocalAddr: ");
        }

        void getPeerAddr(Socket s, Addr& addr) {
            socklen_t len = sizeof(addr);
            check(getpeername(s, (sockaddr*)&addr, &len), "getPeerAddr: ");
        }

        Socket makeSocket(const AddrInfo& info) {
            return check(socket(info.ai_family, info.ai_socktype,
                                info.ai_protocol));
        }

        void bindSocket(Socket s, const AddrInfo& info) {
            check(bind(s, info.ai_addr, info.ai_addrlen), "bindSocket: ");
        }

        void listen(Socket s, int backlog) {
            check(::listen(s, backlog), "listen: ");
        }

        Socket acceptSocket(Socket s, Addr& addr) {
            socklen_t sz = sizeof(addr);
            return check(accept(s, (sockaddr*)&addr, &sz), "acceptSocket: ");
        }

        void connectSocket(Socket s, const AddrInfo& info) {
            check(connect(s, info.ai_addr, info.ai_addrlen), "connectSocket: ");
        }

        Socket listenTo(const char* service, bool reuseAddr, int backlog,
                        bool loopback) {
            AddrInfo* info;
            if (loopback)
                info = getLoopbackAddrInfo(service);
            else
                info = getLocalAddrInfo(service);
            AddrInfoGuard guard(info);
            Socket listener = makeSocket(*info);
            if (reuseAddr)
                setReuseAddr(listener);
            bindSocket(listener, *info);
            listen(listener, backlog);
            return listener;
        }

        Socket connectTo(const char* host, const char* service) {
            AddrInfo* info = getAddrInfo(host, service);
            AddrInfoGuard guard(info);
            Socket conn = makeSocket(*info);
            connectSocket(conn, *info);
            return conn;
        }

        void shutdownSocket(Socket s, int how) {
            check(shutdown(s, how), "shutdownSocket: ");
        }

        // todo: destructors shouldn't throw...
        void closeSocket(Socket s) {
            check(closesocket(s), "closeSocket: ");
        }

        void setNoDelay(Socket s, int val) {
            check(setsockopt(s, IPPROTO_TCP, TCP_NODELAY,
                             (const optval*)&val, sizeof(int)), "setNoDelay: ");
        }

        void setReuseAddr(Socket s, int val) {
            check(setsockopt(s, SOL_SOCKET, SO_REUSEADDR,
                             (const optval*)&val, sizeof(int)),
                  "setReuseAddr: ");
        }

//         // is this BSD only?
//         int setNoSigPipe(Socket s, int val) { // sigpipe doesn't exist in win32
//             return 0;
// //             return setsockopt(s, SOL_SOCKET, SO_NOSIGPIPE, (const void*)&val,
// //                               sizeof(int));
//         }

        void setBlocking(Socket s) {
#ifdef PLATFORM_POSIX
            check(fcntl(s, F_SETFL, (fcntl(s, F_GETFL))&(~O_NONBLOCK)),
                  "setBlocking: ");
#elif defined(PLATFORM_WIN32)
            u_long val = 0;
            check(ioctlsocket(s, FIONBIO, &val), "setBlocking: ");
#endif
        }

        void setNonBlocking(Socket s) {
#ifdef PLATFORM_POSIX
            check(fcntl(s, F_SETFL, (fcntl(s, F_GETFL))|O_NONBLOCK),
                  "setNonBlocking: ");
#elif defined(PLATFORM_WIN32)
            u_long val = 1;
            check(ioctlsocket(s, FIONBIO, &val), "setNonBlocking: ");
#endif
        }

        // todo: MSG_NOSIGNAL ?
        int recv(Socket s, char* data, unsigned length, int flags) {
            return check(::recv(s, data, length, flags), "recv: ");
        }

        int send(Socket s, const char* data, unsigned length, int flags) {
            return check(::send(s, data, length, flags), "send: ");
        }

        void sendAll(Socket s, const char* data, unsigned& length, int flags) {
            int remaining = length;
            while (remaining > 0) {
                int result = ::send(s, data, remaining, flags);
                if (result == -1) {
                    length -= remaining;
                    sysErr("sendAll: ");
                }
                data += result;
                remaining -= result;
            }
        }

        int recvFrom(Socket s, Addr& addr, char* data, unsigned length,
                     int flags) {
            socklen_t sz = sizeof(addr);
            return check(recvfrom(s, data, length, flags, (sockaddr*)&addr,
                                  &sz), "recvFrom: ");
        }

        int sendTo(Socket s, const Addr& addr, const char* data,
                   unsigned length, int flags) {
            return check(sendto(s, data, length, flags, (const sockaddr*)&addr,
                                sizeof(addr)),"sendTo: ");
        }

        // terrible duplication
        void sendAllTo(Socket s, const Addr& addr, const char* data,
                       unsigned& length, int flags) {
            int remaining = length;
            while (remaining > 0) {
                int result = sendto(s, data, remaining, flags,
                                    (const sockaddr*)&addr, sizeof(addr));
                if (result == -1) {
                    length -= remaining;
                    sysErr("sendAllTo: ");
                }
                data += result;
                remaining -= result;
            }
        }
    }
}
