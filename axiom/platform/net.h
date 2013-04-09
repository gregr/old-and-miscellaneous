#ifndef axiom_platform_net_H_
#define axiom_platform_net_H_

#include <string>
#ifdef PLATFORM_POSIX
#include <netdb.h>
#elif defined(PLATFORM_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <ws2tcpip.h>
// errno constants that are commonly tested against should be defined somewhere
// how about in error.h?
#define EWOULDBLOCK WSAEWOULDBLOCK
#else
#error "axiom_platform_net: unsupported platform"
#endif

// todo: make use of SystemError to simplify error-reporting interface
namespace platform {
    namespace net {

        void init();
        void quit();

        struct NetInitGuard {
            NetInitGuard() { init(); }
            ~NetInitGuard() { quit(); }
        private:
            // non-copyable
            NetInitGuard(const NetInitGuard&);
            void operator=(const NetInitGuard&);
        };

        const int tcpSocketType = SOCK_STREAM;
        const int udpSocketType = SOCK_DGRAM;

        typedef int Socket;
        typedef sockaddr_storage Addr;
        typedef addrinfo AddrInfo;

        std::string getHostName();

        std::string getAddrHost(const Addr&);
        std::string getAddrService(const Addr&);

        inline std::string addrRepr(const Addr& addr) {
            return getAddrHost(addr)+":"+getAddrService(addr);
        }

        const AddrInfo* nextAddrInfo(const AddrInfo&);
        const Addr& getAddr(const AddrInfo&);

        // for connecting to a remote location
        AddrInfo* getAddrInfo(const char* host, const char* service,
                              int socketType=tcpSocketType,
                              int family=AF_UNSPEC, int flags=0);
        // for binding and accepting remote connections
        AddrInfo* getLocalAddrInfo(const char* service,
                                   int socketType=tcpSocketType,
                                   int family=AF_UNSPEC, int flags=0);
        // for internal communication
        AddrInfo* getLoopbackAddrInfo(const char* service,
                                      int socketType=tcpSocketType,
                                      int family=AF_UNSPEC, int flags=0);
        // must be called for every AddrInfo* returned by above calls
        void freeAddrInfo(AddrInfo*);

        struct AddrInfoGuard {
            AddrInfoGuard(AddrInfo* info) : i_(info) {}
            ~AddrInfoGuard() { freeAddrInfo(i_); }
        private:
            // non-copyable
            AddrInfoGuard(const AddrInfoGuard&);
            void operator=(const AddrInfoGuard&);
            AddrInfo* i_;
        };

        void getLocalAddr(Socket, Addr&);
        void getPeerAddr(Socket, Addr&);

        const int defaultBacklog = 10;

        Socket makeSocket(const AddrInfo&);
        void bindSocket(Socket, const AddrInfo&);
        void listen(Socket s, int backlog=defaultBacklog);
        Socket acceptSocket(Socket, Addr&);
        void connectSocket(Socket, const AddrInfo&);

        // convenient functions to perform usual setup for servers/clients
        // if there is an error, the result will be -1 and either aiStatus will
        // be nonzero or errno will be set, depending on what actually failed
        Socket listenTo(const char* service, bool reuseAddr=false,
                        int backlog=defaultBacklog, bool loopback=false);
        Socket connectTo(const char* host, const char* service);

        // unix and windows have different constant names
        // 0: SHUT_RD, SD_RECV
        // 1: SHUT_WR, SD_SEND
        // 2: SHUT_RDWR, SD_BOTH
        // by default, shutdown writing to notify peer of impending close
        void shutdownSocket(Socket s, int how=1);
        void closeSocket(Socket);

        struct SocketGuard {
            SocketGuard(Socket s) : s_(s) {}
            ~SocketGuard() { closeSocket(s_); }
        private:
            // non-copyable
            SocketGuard(const SocketGuard&);
            void operator=(const SocketGuard&);
            Socket s_;
        };

        void setNoDelay(Socket s, int val=1);
        void setReuseAddr(Socket s, int val=1);
        //        int setNoSigPipe(Socket s, int val=1);
        void setBlocking(Socket);
        void setNonBlocking(Socket);

        int recv(Socket s, char* data, unsigned len, int flags=0);
        int send(Socket s, const char* data, unsigned len, int flags=0);
        void sendAll(Socket s, const char* data, unsigned& length, int flags=0);
        int recvFrom(Socket s, Addr& addr, char* data, unsigned length,
                     int flags=0);
        int sendTo(Socket s, const Addr& addr, const char* data,
                   unsigned length, int flags=0);
        void sendAllTo(Socket s, const Addr& addr, const char* data,
                       unsigned& length, int flags=0);
    }
}

#endif
