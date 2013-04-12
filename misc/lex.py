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

__all__ = ["Error", "tokenType", "Lexer"]

from uriel.util.misc import InputReader, NameMap as Enum


tokenType = Enum(["eof", "newline", "whitespace", "symbol", "literal"])
escapeChar = "\\"
stringQuote = "\""

class Error(StandardError): pass

def _errorMsg(msg, state):
    return "%s: line %s, column %s" % (msg, state.lineNum, state.current-1)


# probably less efficient than the joined stream/state
# but it's cleaner to separate the logic into InputReader and this new class
class ReadState(object):
    def __init__(self, charStream):
        self.stream = InputReader(charStream)
        self.symbol = []
        self.lineNum = 0
        self.escaped = False
    def peek(self):
        return self.stream.peek()
    def next(self):
        ch = self.stream.next()
        self.symbol.append(ch)
        if ch == '\n':
            self.lineNum += 1
        return ch
    def prev(self):
        self.stream.prev()
        self.symbol.pop() 
    def omit(self):
        self.next()
        self.symbol.pop()
    def inProgress(self):
        return bool(self.symbol)
    def popSymbol(self):
        value = "".join(self.symbol)
        self.symbol = []
        return value


class _ReadState(object):
    def __init__(self, charStream):
        self.stream = charStream
        self.acc = []
        self.lineNum = 0
        self.escaped = False
        self._readNextLine()
    def next(self):
        ch = self.peek()
        self.current += 1
        return ch
    def prev(self):
        assert self.start < self.current
        self.current -= 1
    def peek(self):
        if self.current >= len(self.line):
            self._commit()
            self._readNextLine()
        return self.line[self.current]
    def omit(self):
        assert self.start < self.current
        self._commit(1)
    def pop(self):
        self._commit()
        value = "".join(self.acc)
        self.acc = []
        return value
    def inProgress(self, omissions=0): # is a symbol being constructed?
        return ((self.current-omissions != self.start) or
                self.acc)
    def _readNextLine(self):
        line = self.stream.readline()
        if not line:
            raise EOFError
        self.line = line
        self.lineNum += 1
        self.start = self.current = 0
    def _commit(self, numOmitted=0):
        current = self.current
        self.acc.extend(self.line[self.start:current-numOmitted])
        self.start = current


def readWhitespace(state, ch): # collect an indentation token
    while True:
        if ch == "\n":
            state.popSymbol() # scrap any trailing whitespace
            return (tokenType.newline,)
        ch = state.peek()
        if not ch.isspace():
            break
        state.next()
    return (tokenType.whitespace, state.popSymbol())


def readEscape(state, ch):
    state.omit()
    try:
        ch = state.peek()
        if ch == "\n":
            state.omit()
        else:
            state.next()
            validateConstituent(ch)
            state.escaped = True
    except EOFError:
        raise Error, _errorMsg("invalid escape sequence", state)


def readStringQuote(state, ch):
    state.omit()
    lastSymbol = getSymbol(state)
    try:
        while True:
            ch = state.peek()
            if ch == escapeChar:
                readEscape(state, ch)
            elif ch == stringQuote:
                state.omit()
                break
            else:
                state.next()
                validateConstituent(ch)
        return (lastSymbol, getSymbol(state, True))
    except EOFError:
        raise Error, _errorMsg("invalid string sequence", state)


def getSymbol(state, stringLiteral=False):
    escaped = state.escaped
    state.escaped = False
    symbol = state.popSymbol()
    if stringLiteral:
        return (tokenType.literal, symbol)
    if symbol:
        if not escaped:
            try:
                literal = eval(symbol)
                if type(literal) in [int, long, float, complex]:
                    return (tokenType.literal, literal)
            except:
                pass
        return (tokenType.symbol, symbol)


def validateConstituent(ch):
    code = ord(ch)
    if (code < 32) or (code > 126):
        if not ch.isspace():
            raise Error, _errorMsg(("invalid character with code: %s" % code),
                                   state)


def readComment(state, ch):
    state.omit()
    symbol = getSymbol(state)
    try:
        while state.next() != "\n":
            pass
    except EOFError:
        pass # end of file also obviously means end of comment
    state.popSymbol() # scrap the rest of the line
    return (symbol, (tokenType.newline,))


def readSyntacticChar(state, ch):
    state.omit()
    return (getSymbol(state), (ch,))


# eventually implement multi-line comments too
# character that combines with commentChar to extend comment to multiple lines
def _makeLexer(commentChar, commentExtensionChar):
    macros = {commentChar:readComment,
              escapeChar:readEscape,
              stringQuote:readStringQuote}

    def readSpecialChar(state, ch):
        if not state.inProgress(): # 1 omission for the character just read
            # check for whitespace or a macro char directly after this char
            state.next()
            next = state.peek()
            if next.isspace() or next in macros:
                state.prev()
                state.omit()
                return ((ch,),) # it's not part of a symbol, so it's special

    # these are terminating macro characters
    def addSyntacticChars(syntacticChars):
        """ """
        macros.update((c, readSyntacticChar) for c in syntacticChars)

    # a special character won't terminate a symbol if it appears as part of a
    # block of constituents
    def addSpecialChars(specialChars):
        """ """
        macros.update((c, readSpecialChar) for c in specialChars)

    def _rawTokenStream(charStream):
        try:
            state = ReadState(charStream)
            while True:
                ch = state.peek()
                if ch in macros:
                    result = macros[ch](state, ch)
                    if result is not None:
                        for token in result:
                            yield token
                elif ch.isspace():
                    yield getSymbol(state)
                    state.next()
                    yield readWhitespace(state, ch)
                else:
                    state.next()
                    validateConstituent(ch)
        except EOFError:
            yield getSymbol(state)
            yield (tokenType.eof,)

    def tokenStream(charStream):
        """ """
        for t in _rawTokenStream(charStream):
            if t is not None:
                yield t


    return addSyntacticChars, addSpecialChars, tokenStream


class Lexer(object):
    """
    """
    defaultCommentChar = "#"
    defaultCommentExtChar = ""
    def __init__(self, commentChar=defaultCommentChar,
                 commentExtChar=defaultCommentExtChar):
        """ """
        self.addSyntacticChars, self.addSpecialChars, self.tokenStream = _makeLexer(commentChar,
                                                                                    commentExtChar)


# for interactive testing
def _interact(lexer=Lexer()):
    import sys
    for t in lexer.tokenStream(sys.stdin):
        if t is not None:
            print t


#target = """This is an (example of what ) the  \tlexer sh\)ould  
#somehow\\
#be |able to "rec0gn(ize)|, assuming  \\"  (it) w0rks of course,\\
#\t duuuuh." man)... oh and btw # this is a comment
#yeah so
#"""

#from StringIO import StringIO

#tokens = tokenStream(StringIO(target))

#ts = [t for t in tokens]

#    print ts

if __name__ == "__main__":
    lexer = Lexer()
    # just for testing, hardwire these characters
    lexer.addSyntacticChars(['(', ')'])
    lexer.addSpecialChars(['.'])
    _interact(lexer)
