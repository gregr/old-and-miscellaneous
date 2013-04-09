#include "thread.h"
#include "select.h"
#include "net.h"
#include "error.h"
#include <iostream>
#include <stdio.h>
#include <errno.h>
using namespace platform::net;
using namespace platform::thread;
using namespace platform;
using namespace std;

FdSet allClients;
ScopedMutex clientMutex, ioMutex;

struct ClientConn {
    Addr addr;
    Socket s;
};

void broadcast(Socket origin, const char* msg, unsigned len) {
    ScopedLock cl(clientMutex);
    ScopedLock il(ioMutex);
    printf("client %d said: %s\n", origin, msg);
    FdSetIter itr(allClients);
    for (int s = *itr; s != allClients.end(); s = ++itr) {
        if (s != origin)
            send(s, msg, len);
    }
}

void* ThreadFuncCC handleClient(void* arg) {
    // so fugly
    ClientConn* cp = (ClientConn*)arg;
    ClientConn client = *cp;
    delete cp; // get rid of this toxic waste asap
    try {
        for (;;) {
            const unsigned bufferLen = 256;
            char buffer[bufferLen]; // gross
            unsigned len = recv(client.s, buffer, bufferLen);
            if (len == 0)
                break;
            if (len+2 > bufferLen) {
                throw SystemError("buffer overflow protection\n", len);
            }
            buffer[len] = '\n';
            buffer[len+1] = 0;
            // broadcast
            broadcast(client.s, buffer, len+1);
        }
    } catch (const std::exception& e) {
        ScopedLock il(ioMutex);
        printf("exception: %s\n", e.what());
    } catch (...) {
        ScopedLock il(ioMutex);
        printf("unknown exception\n");
    }
    {
        ScopedLock cl(clientMutex);
        allClients.remove(client.s);
    }
    {
        ScopedLock il(ioMutex);
        printf("client %d disconnected\n", client.s);
    }
    return 0;
}

void runServer(const char* service) {
    Socket listener = listenTo(service);
    SocketGuard lg(listener);
    for (;;) {
        ClientConn* client = new ClientConn; // yuck, new
        client->s = acceptSocket(listener, client->addr);
        {
            ScopedLock cl(clientMutex);
            allClients.add(client->s);
        }
        {
            ScopedLock il(ioMutex);
            printf("client %d connected: %s\n", client->s,
                   addrRepr(client->addr).c_str());
        }
        start(handleClient, (void*)client);
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
        if (argc == 2)
            runServer(argv[1]); // server
        else if (argc == 3)
            runClient(argv[1], argv[2]); // client
        else
            printf("insufficient arguments\n");
    } catch (const SystemError& e) {
        ScopedLock il(ioMutex);
        printf("SystemError: %s, %d\n", e.what(), e.code);
    } catch (const std::exception& e) {
        ScopedLock il(ioMutex);
        printf("exception: %s\n", e.what());
    } catch (...) {
        ScopedLock il(ioMutex);
        printf("unknown exception\n");
    }
    return 0;
}
