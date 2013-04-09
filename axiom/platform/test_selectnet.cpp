#include "select.h"
#include "net.h"
#include "error.h"
#include <iostream>
#include <stdio.h>
#include <errno.h>
using namespace platform::net;
using namespace platform;
using namespace std;

void runServer(const char* service) {
    FdSet clients;
    //    Socket listener = listenTo(service, true);
    Socket listener = listenTo(service);
    SocketGuard lg(listener);
    for (;;) {
        FdSet rfds = clients;
        rfds.add(listener);
        int count = select(&rfds);
        if (count > 0) { // should only be 0 if there's a timeout
            FdSetIter itr(rfds);
            for (int s = *itr; s != rfds.end(); s = ++itr) {
                printf("selected %d\n", s);
                if (s == listener) {
                    Addr incoming;
                    Socket client = acceptSocket(listener, incoming);
                    clients.add(client);
                    printf("client %d connected: %s\n", client,
                           addrRepr(incoming).c_str());
                } else {
                    const unsigned bufferLen = 256;
                    char buffer[bufferLen]; // todo: this is crap
                    unsigned len = recv(s, buffer, sizeof(buffer));
                    if (len == 0) { // disconnection
                        clients.remove(s);
                        printf("client %d disconnected\n", s);
                    } else {
                        if (len+2 > bufferLen) {
                            throw SystemError("buffer overflow protection\n",
                                              len);
                        }
                        buffer[len] = '\n';
                        buffer[len+1] = 0;
                        // broadcast
                        printf("client %d said: %s\n", s, buffer);
                        FdSetIter itr(clients);
                        for (int ss = *itr; ss != clients.end(); ss = ++itr) {
                            if (s != ss)
                                send(ss, buffer, len+1);
                        }
                    }
                }
            }
        }
    }
}

void runClient(const char* host, const char* service) {
    Socket conn = connectTo(host, service);
    setNonBlocking(conn);
    string input;
    while (getline(cin, input)) {
        if (input != "") {
            send(conn, input.c_str(), input.length());
        }
        const unsigned bufferLen = 256;
        char buffer[bufferLen]; // todo: this is crap
        try {
            unsigned len = recv(conn, buffer, bufferLen);
            if (len == 0) {// disconnect
                printf("server closed connection\n");
                return;
            } else {
                if (len+1 > bufferLen) {
                    throw SystemError("buffer overflow protection\n", len);
                }
                buffer[len] = 0;
                printf("%s\n", buffer);
            }
        } catch (const SystemError& e) {
            if (e.code != EWOULDBLOCK)
                throw;
        }
    }
}

int main(int argc, char** argv) {
    try {
        NetInitGuard net;
        if (argc == 2)
            runServer(argv[1]); // server
        else if (argc == 3)
            runClient(argv[1], argv[2]); // client
        else
            printf("insufficient arguments\n");
    } catch (const SystemError& e) {
        printf("SystemError: %s, %d\n", e.what(), e.code);
    } catch (const std::exception& e) {
        printf("exception: %s\n", e.what());
    } catch (...) {
        printf("unknown exception\n");
    }
    return 0;
}
