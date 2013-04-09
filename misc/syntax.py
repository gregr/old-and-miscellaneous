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

# TODO: makeExpr and other syntaxer/lexer functionality can be wrapped
# up nicely and exposed to the user for language extensions

__all__ = ["Error", "Syntaxer"]

from lex import tokenType, Lexer
from core import *
from uriel.util.misc import Stream
from weakref import proxy
from functools import partial


class Error(StandardError): pass

def _errorMsg(msg, state):
    return "%s: line %s" % (msg, state.lineNum)

def checkForUnexpectedToken(state, token, expected):
    if token != expected:
        raise Error, _errorMsg("unexpected '%s'" % token, state)


class PairingToken(object):
    pass


class EndExpr(object):
    def __init__(self, token):
        self.token = token

def endExpr(endToken, state):
    return EndExpr(endToken)
#    raise EndExpr(endToken)


def whitespaceEliminatingTokenStream(tokens):
    for t in tokens:
        ttype = t[0]
        # filter out whitespace
        if (ttype != tokenType.whitespace) and (ttype != tokenType.newline):
            yield t
        

def printingTokenStream(tokens):
    for t in tokens:
        print t
        yield t


def lineCounterTokenStream(state, tokens):
    for t in tokens:
        if t[0] == tokenType.newline:
            state.lineNum += 1
        yield t


def makeLineCounterTokenStream(state, tokens):
    state.lineNum = 1
    return lineCounterTokenStream(proxy(state), tokens)


class State(object):
    def __init__(self, tokens, tokenProcessor):
        tokens = Stream(makeLineCounterTokenStream(self, tokens))
#        tokens = Stream(printingTokenStream(makeLineCounterTokenStream(self, tokens)))
#        tokens = Stream(printingTokenStream(tokenProcessor(tokens)))
        tokens = Stream(tokenProcessor(tokens))
        self.tokens = tokens


class InfixOperator(object):
    def __init__(self, symbolName, rightAssoc=True):
        self.symbol = Symbol(symbolName)
        self.rightAssoc = rightAssoc
    def __str__(self):
        return str(self.symbol)
    def __repr__(self):
        return repr(self.symbol)


openImplicitExprToken = "(("
closeImplicitExprToken = "))"


def beginIndentedExprTokens(tokens, indents):
    for t in tokens:
        indent = 0
        while t[0] == tokenType.whitespace: # determine indentation
            indent += len(t[1])
            t = tokens.next()
        if t[0] == tokenType.newline: # if the line is blank, skip it
            continue
        if t[0] == tokenType.eof: # the end?
            eofToken = t
            for t in closeAllIndentedExprTokens(indents):
                yield t
            yield eofToken
            break
        tokens.put(t) # found first non-whitespace non-newline
        while indents and (indent <= indents[-1]): # close all exprs indented farther
            indents.pop()
            yield (closeImplicitExprToken,)
        if (not indents) or (indent > indents[-1]): # open new indented expr
            indents.append(indent)
        yield (openImplicitExprToken,) # begin indented expr
        break


def closeAllIndentedExprTokens(indents):
    while indents:
        indents.pop()
        yield (closeImplicitExprToken,)


def _makeSyntaxer(openExprToken, closeExprToken, pairingToken, **kwargs):

    def continueIndentedExprTokens(tokens, indents):
        for t in tokens:
            ttype = t[0]
            if ttype != tokenType.whitespace: # internal whitespace isn't important
                if ttype == tokenType.newline: # when a line ends a new indented expr is started
                    for t in beginIndentedExprTokens(tokens, indents):
                        yield t
                elif ttype == tokenType.eof: # immediate eof
                    eofToken = t
                    for t in closeAllIndentedExprTokens(indents):
                        yield t
                    yield eofToken
                else: # yield all other tokens
                    yield t
                    if ttype == openExprToken: # but leave this state when starting a parenthesized expr
                        break


    def continueParenthesizedExprTokens(tokens):
        for t in tokens:
            ttype = t[0]
            if (ttype != tokenType.whitespace) and (ttype != tokenType.newline): # filter out whitespace
                yield t
                if ttype == closeExprToken: # until the parenthesized expr is completed
                    break


    def whitespaceProcessingTokenStream(tokens):
        indents = []
        for t in beginIndentedExprTokens(tokens, indents): # this begins the initial indented expr
            yield t
        while True:
            for t in continueIndentedExprTokens(tokens, indents): # this continues until finding openExprToken
                yield t
            for t in continueParenthesizedExprTokens(tokens): # this continues until finding closeExprToken
                yield t


    infixOps = {}

    def makeInfixOp(symbol, rightAssoc=True):
        infixOps[symbol] = InfixOperator(symbol, rightAssoc)


    macros = {}

    def makeAtom(state, token):
        ttype = token[0]
        if ttype == tokenType.literal:
            return token[1]
        elif ttype == tokenType.symbol:
            tvalue = token[1]
            if tvalue in infixOps:
                return infixOps[tvalue]
            return Symbol(tvalue)
        elif ttype == tokenType.eof:
            raise Error, _errorMsg("unexpected eof", state)
        else:
            raise Error, _errorMsg("unexpected token '%s'" % token, state)




    def makeTerm(state):
        token = state.tokens.next()
        ttype = token[0]
        if ttype in macros:
            term = macros[ttype](state)
        elif ttype == pairingToken:
            term = PairingToken()
        else:
            term = makeAtom(state, token)
        assert term is not None
        return term


    def makeInfixExpr(state, leftTerm, infixOp, expectedEndToken):
        rightTerm = None
        try:
            rightTerm = makeTerm(state)
            if type(rightTerm) == InfixOperator:
                raise Error, _errorMsg("unexpected operator '%s'" % rightTerm, state)
            infix = makeTerm(state) # this should raise EndExpr or return an infix op if correctly formed
            if type(infix) != InfixOperator:
                raise Error, _errorMsg("improperly formed infix expression", state)
            if infixOp.rightAssoc: # right associative
                rightTerm = makeInfixExpr(state, rightTerm, infix, expectedEndToken)
            else: # left associative
                expr = makeList(infixOp.symbol, leftTerm, rightTerm)
                return makeInfixExpr(state, expr, infix, expectedEndToken)
        except EndExpr, e:
            checkForUnexpectedToken(state, e.token, expectedEndToken)            
            if rightTerm is None:
                raise Error, _errorMsg("insufficient arguments to operator '%s'" % infixOp, state)
        return makeList(infixOp.symbol, leftTerm, rightTerm)


    # it's a hack to just hardcode the value of quote, but i'm getting sick
    # of how this syntaxer was designed
    quotedExprToken = "'"
    def makeQuotedExpr(state):
        return makeList(Symbol("quote"), makeTerm(state))


    def makeExpr(expectedEndToken, state):
        expr = _makeExpr(expectedEndToken, state)
        if not isList(expr):
            raise Error, _errorMsg("invalid paired form, '%s' cannot appear first in expression" % pairingToken, state)
        return expr


    def _makeExpr(expectedEndToken, state):
        term = makeTerm(state)
        if isinstance(term, EndExpr):
            checkForUnexpectedToken(state, term.token, expectedEndToken)
            return nil
        elif isinstance(term, PairingToken):
            term = makeTerm(state)
            endTerm = makeTerm(state)
            if not isinstance(endTerm, EndExpr):
                raise Error, _errorMsg("invalid paired form, one term must follow '%s' before end of expression" % pairingToken, state)
            return term
        return makePair(term, _makeExpr(expectedEndToken, state))
            # this was the old iterative way... but it's really annoying
            # better to use an elegant recursive method
#        try:
#            while True:
#                term = makeTerm(state)
#                if type(term) == InfixOperator:
#                    if len(terms) == 0: # if a unary expr, evaluate infix as its own symbol
#                        try:
#                            makeTerm(state) # this MUST raise EndExpr to be a valid unary expr
#                        except EndExpr:
#                            return term.symbol
#                    elif len(terms) == 1:
#                        lastTerm = terms.pop()
#                        return makeInfixExpr(state, lastTerm, term, expectedEndToken)
#                    raise Error, _errorMsg("improperly formed infix expression", state)
#                terms.append(term)
#        except EndExpr, e:
#            checkForUnexpectedToken(state, e.token, expectedEndToken)
#        numTerms = len(terms)
#        if numTerms == 0:
#            return nil
#        return makeList(*terms)


    def makeIndentedExpr(expectedEndToken, state):
        expr = makeExpr(expectedEndToken, state)
        # indented exprs containing only one term should become that term
        if isPair(expr) and (pairTail(expr) is nil):
            return pairHead(expr)
        return expr


    macros.update({ openExprToken:partial(makeExpr, closeExprToken),
                    closeExprToken:partial(endExpr, closeExprToken),
                    quotedExprToken:makeQuotedExpr
                    })
#, "\'":None, "`":None, ",":None}

    macros.update(kwargs)
    # these macros should probably not get to the lexer, but whatever
    macros.update({openImplicitExprToken:partial(makeIndentedExpr, closeImplicitExprToken),
                   closeImplicitExprToken:partial(endExpr, closeImplicitExprToken)})

    return macros, makeTerm, makeInfixOp, whitespaceProcessingTokenStream


def _rawExprStream(whitespaceProcessor, makeTerm, tokens):
    state = State(tokens, whitespaceProcessor)
    try:
        while True:
            token = state.tokens.next()
            if token[0] == tokenType.eof:
                break
            state.tokens.put(token)
            yield makeTerm(state)
    except StopIteration:
        raise Error, _errorMsg("unexpected token stream termination", state)


class Syntaxer(object):
    def __init__(self, processWhitespace=True,
                 openExprToken="(", closeExprToken=")", pairingToken=".",
                 **macros):
        self.pairingToken = pairingToken
        (self.macros, self.makeTerm, self.makeInfixOp,
         whitespaceProcessor) = _makeSyntaxer(openExprToken, closeExprToken, pairingToken, **macros)
        if processWhitespace:
            self.whitespaceProcessor = whitespaceProcessor
        else:
            self.whitespaceProcessor = whitespaceEliminatingTokenStream
    def makeLexer(self, commentChar=Lexer.defaultCommentChar,
                 commentExtChar=Lexer.defaultCommentExtChar):
        lexer = Lexer(commentChar, commentExtChar)
        lexer.addSyntacticChars(self.macros.keys())
        lexer.addSpecialChars([self.pairingToken]) # just one for now
        return lexer
    def exprStream(self, tokens):
        return _rawExprStream(self.whitespaceProcessor, self.makeTerm, tokens)


#_infixOpsString = ": :: ->" # : and -> are definitely right-associative... what about others?
# just treat as symbols separated by spaces
# and have a special check for symbols in the "infixOps" table

# right-associative ':' and other infix binops
# expr => term infixOp expr
# expr => term expr
# expr => nothing # nil

# term => (expr) # and other macro-handled forms like quote
# term => literal
# term => symbol
# term => infixOp

# get next token
# until eof
# while whitespace, add indentation
# hmm...


# infix operator as first element in list form takes 0 arguments and returns prefix-form function? NO
# a binary function symbol prefixed with $ in miranda becomes infix right-assoc
# infix op prefixed with something else makes it a regular prefix-form binary func?
# mult/div is left assoc, +/- is right..? that doesn't make sense for -


def Parser(syntaxer, lexer=None):
    if lexer is None:
        lexer = syntaxer.makeLexer()
    def parse(charStream):
        return syntaxer.exprStream(lexer.tokenStream(charStream))
    return parse


def _parseProg(prog, processWhitespace=False, printExprs=False):
    out = []
    syntaxer = Syntaxer(processWhitespace=processWhitespace)
    syntaxer.makeInfixOp(":")
    #makeInfixOp("->")
    parse = Parser(syntaxer)
    for expr in parse(prog):
        out.append(expr)
        if printExprs:
            print expr
    return out


# for interactive testing
def _interact():
    import sys
    return _parseProg(sys.stdin)


from StringIO import StringIO

testProg = StringIO("""def func (a b c)
  (* a (+ b c))

func 3 2 1
a
((:) 1)
(:) 1
1 : 2 : 3
(4 : 5 : 6)
print ("hello" ++ "world")
7 : 8 : 9""")


def _test(prog=testProg):
    return _parseProg(prog)


if __name__ == "__main__":
    _interact()
