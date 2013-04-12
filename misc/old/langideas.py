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

import re

digit = "0-9"

alpha = "a-zA-Z"

alphaNum = alpha+digit

constituentSymbols = r"!$%&*+\-./:<=>?@\[\]^_{}~" # plus digits and alpha, backspace, and rubout

terminating = r"\"'(),;`"

nonterminating = "#"

reserved = r"\[\]{}!?"

singleescape = r"\\"

multipleescape = "|"

floatNumber = re.compile(r'[+\-]?(\d*\.?\d+)|(\d+\.?)([eE][+\-]?\d+)?')

integer = re.compile(r'[+\-]?\d+')

#floatRatio = re.compile(r'') # ((integer|floatNumber)/floatNumber)|(floatNumber/(integer|floatNumber))

#integerRatio = # integer/integer

#complexNumber = re.compile() # (floatNumber[+-])?floatNumber[jJ]


lexMacros = {'+':None, '-':None, '.':None, '#':None, '\\':None, '|':None}
# comments are done through a macro


class ReadState:
    def __init__(self, stream):
        self.stream = stream
        self.readNextLine()
    def readNextLine(self):
        line = self.stream.readline()
        if not line:
            raise EOFError
        self.line = line+'\n'
        self.start = self.current = 0
    def next(self):
        assert self.current < len(self.line)
        ch = self.line[self.current]
        self.current += 1
        return ch
    def putback(self):
        assert self.current >= self.start
        self.current -= 1
    def pop(self):
        current = self.current
        acc = self.line[self.start:current]
        self.start = current
        return acc


def readSymbol(state):
    pass


def readNumberOrSymbol(state):
    pass


def readToken(state):
    try:
        ch = state.next()
#        if ch == '\n': # this can't happen at the start because EOFError would have been raised
#            pass
        if ch.isspace():
            while True:
                ch = state.next()
                if not ch.isspace():
                    return # return an indent token
                if ch == "\n":
                    # save whitespace token
                    # state.readNextLine()
                    return # return an indent token
        elif ch in lexMacros:
            pass
        elif ch in ["\\", "|"]:
            return readSymbol(state)
        else: # constituent
            return readNumberOrSymbol(state)
    except EOFError:
        pass


def tokenStream(stream):
    for line in stream:
        start = current = 0
        end = len(line)
        while True:
            if current >= end:
                if start != current:
                    yield makeToken(line[start:current])
                break
            ch = line[current]
            if ch in lexMacros:
                if start != current:
                    yield makeToken(line[start:current])
                start = current
                current = lexMacros[ch](line, current)
            elif ch.isspace():
                if start != current:
                    yield makeToken(line[start:current])
                current = skipWhitespace(line, current)
            else:
                current += 1
        

def accmulate(stream):
    symbol = []
    while True:
        ch = stream.get()
        if not ch:
            pass
        elif ch in lexMacros:
            stream.put(ch)
            lexMacros[ch](stream)
        elif ch.isspace():
            clearWhitespace(stream)
        else:
            symbol.append(ch)
            continue
        break


from collections import deque


class Stream(object):
    def __init__(self, source):
        self.source = source
        self.buf = deque()
    def get(self):
        if not self.buf:    # buffer use/filling can be optimized later
            return self.source.read(1)
        return self.buf.popleft()
    def put(self, ch):
        self.buf.append(ch)



def makeToken(s):
    if s:
        pass


class Source:
    def __init__(self, source):
        self.source = source
        self.head = 0
        self.tail = 0
        self.end = len(source)
    def look(self, offset=0):
        try:
            return self.source[self.tail+offset]
        except IndexError:
            return None
    def accumulate(self):
        self.tail += 1
    def popAccmulated(self):
        head = self.head
        tail = self.tail
        self.head = tail
        return self.source[head:tail]
    def __nonzero__(self):
        return self.tail < self.end
    def __len__(self):
        l = self.end-self.tail
        if l < 0:
            l = 0
        return l


def anotherRead(source, current):
    start = current
    try:
        ch = source[current]
        while (not ch.isspace()) and (ch not in lexMacros):
            current += 1
        makeToken(source[start:current])
        start = current
    except IndexError:
        pass # end of source


def isNumber(source):
    pass


def lex(source):
    tokens = []
    source = Source(source)
    while source:
        ch = source[0]
        if ch in lexMacros:
            handler = lexMacros[ch]
        else:
            handler = getToken
        token, source = handler(source)
        tokens.append(token)
    return tokens


def getToken(source):
    ch = source[0]
    if ch.isspace():
        return getWhitespace(source)
    elif ch in possibleNumericalChars:
        pass


def getWhitespace(source):
    pass


def getSymbol(source):
    pass


def getNumber(source):
    assert source[0].isdigit()



def parse(source):
    return syntax(lex(source))


def syntax(tokens):
    return tokens


def lex(source):
    tokens = []
    source = source.lstrip()
    while source:
        source = stripComment(source)
        if not source:
            break
        token, source = getToken(source)
        tokens.append(token)
        source = source.lstrip()
    return tokens


reservedChars = ['(', ')']


def stripComment(source):
    ch = source[0]
    if ch == '#':
        index = source.find('\n')
        if index == -1:
            return ""
        return source[index+1:]
    return source


def getToken(source):
    ch = source[0]
    if ch in reservedChars:
        return (source[0], source[1:])
    else:
        return getSymbol(source)


def getSymbol(source):
    index = 0
    length = len(source)
    while index < len:
        ch = source[index]
        if ch.isspace() or ch in reservedChars:
            break;
        index += 1
    return (source[:index], source[index:])
