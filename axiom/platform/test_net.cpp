#include "net.h"
#include <string.h>
using namespace platform::net;

int recvMsg(Socket s) {
    char response[256];
    int result = recv(s, response, sizeof(response));
    if (result > 0)
        printf("received: %s\n", response);
    return result;
}

void recvAll(Socket s) {
    while (recvMsg(s) > 0);
}

void stopConn(Socket s) {
    shutdownSocket(s);
    recvAll(s);
}

void sendMsg(Socket s, const char* msg) {
    unsigned length = strlen(msg)+1;
    sendAll(s, msg, length);
}

void runServer(const char* port) {
    Socket listener = listenTo(port);
    SocketGuard l(listener);
    Addr incoming;
    Socket client = acceptSocket(listener, incoming);
    SocketGuard g(client);
    printf("client connected: %s\n", addrRepr(incoming).c_str());
    sendMsg(client, "hello");
    stopConn(client);
}

void runClient(const char* host, const char* port) {
    Socket conn = connectTo(host, port);
    SocketGuard g(conn);
    recvMsg(conn);
    sendMsg(conn, "hey, what's up");
    stopConn(conn);
}

int main(int argc, const char** argv) {
    try {
        NetInitGuard guard;
        if (argc == 2)
            runServer(argv[1]);
        else
            if (argc == 3)
                runClient(argv[1], argv[2]);
            else
                printf("incorrect number of arguments\n");
    } catch (const std::exception& e) {
        printf("caught exception: %s\n", e.what());
    }
    return 0;
}
