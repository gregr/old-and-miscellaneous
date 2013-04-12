# Copyright (C) 2007 Gregory L. Rosenblatt
# All rights reserved

# <greg.uriel@gmail.com>
# http://code.google.com/p/uriel/

# This file is part of Uriel.

# Uriel is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# Uriel is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this library.  If not, see <http://www.gnu.org/licenses/>

from async import Scheduler
import socket
import errno


def handleSocketError(e):
    value, message = e
    if value not in [errno.EAGAIN, errno.EWOULDBLOCK]:
#        print "throwing"
        raise


def echoHandler(sock):
    while True:
        try:
            data = yield nonBlockingRead(sock)
            if not data:
                yield
            yield nonBlockingWrite(sock, data)
        except socket.error, e:
            yield
#            handleSocketError(e)


def listenOn(scheduler, sock, handler):
    while True:
        connectedSocket = yield nonBlockingAccept(sock)
#        print repr(connectedSocket)
        scheduler.add(handler(connectedSocket))


def tryAccept(sock):
    while True:
#        print "accepting..."
        s = sock.accept()
#        print "accept succeeded", repr(s)
        yield s


def nonBlockingAccept(sock):
    while True:
        try:
#            print "try accept", i[0]
            connectedSocket, address = yield tryAccept(sock)
            connectedSocket.setblocking(0)
            print "client connected:", address
#            raise StandardError, "no"
            yield connectedSocket
        except socket.error, e:
#            print "shit"
            handleSocketError(e)


def tryRead(sock):
    while True:
        yield sock.recv(packetSize)


def nonBlockingRead(sock):
    while True:
        try:
            data = yield tryRead(sock)
#            print "received:", data
            yield data
        except socket.error, e:
            handleSocketError(e)


def tryWrite(sock, data):
    while True:
        yield sock.send(data)


def nonBlockingWrite(sock, data):
    sent = 0
    while True:
        while sent < len(data):
            try:
                print "writing:", data[sent:]
                sent += yield tryWrite(sock, data[sent:])
            except socket.error, e:
                handleSocketError(e)
        yield

packetSize = 4096

def main():
    host = "localhost"
    port = 50001
    backlog = 5
    scheduler = Scheduler()
    listeningSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    listeningSocket.bind((host, port))
    listeningSocket.listen(backlog)
    listeningSocket.setblocking(0)

    server = listenOn(scheduler, listeningSocket, echoHandler)
    scheduler.add(server)
    print "***Starting Echo Server***"
    from time import sleep
    r = scheduler._run()
    for blah in r:
        sleep(0.01)


if __name__ == "__main__":
    main()
