from lex import *
from data import symbol, null, pair
from core import buildExpr, minimalExpr, pairList

class ParseError(StandardError): pass # python already claimed SyntaxError

def parseError(msg):
    raise ParseError, (msg, lineNum)

#syntaxTypes = ["application", "symbol", "value"] # don't need these...

class Stream(object):
    def __init__(self, stream):
        self.buffer = []
        self.stream = stream
    def __iter__(self):
        return self
    def next(self):
        if self.buffer:
            return self.buffer.pop()
        return self.stream.next()
    def put(self, x):
        self.buffer.append(x)
    def empty(self):
        if self.buffer:
            return False
        try:
            self.put(self.stream.next())
        except StopIteration:
            return True
        return False

def makeStream(s):
    if not isinstance(s, Stream):
        s = Stream(s)
    return s

# def filterTokens(tokens, types=("whitespace", "eol")):
#     for t in tokens:
#         if t[0] not in types:
#             yield t

def untilEOL(tokens):
    global lineNum
    for t in tokens:
        if t[0] == "eol":
            lineNum += 1
            break
        elif t[0] != "whitespace":
            yield t

def nextIndent(tokens):
    global lineNum
    indent = 0
    for t in tokens:
        if t[0] == "eol":
            lineNum += 1
            indent = 0
        elif t[0] == "whitespace":
            indent += t[1]
        else:
            tokens.put(t)
            return indent
    return 0 # eos

def indented(tokens):
    tokens = makeStream(tokens)
    while True:
        yield ("syntax", nextIndent(tokens))
        if tokens.empty():
            break
        for t in untilEOL(tokens):
            yield t

def unindented(tokens):
    for t in tokens:
        if t[0] != "syntax" or not isinstance(t[1], int):
            yield t

################ operators ################

def precGT(op, prec):
    return (op.prec > prec) or (op.prec == prec and op.assoc == "right")

def aboveAppPrec(prec):
    return prec is not None and prec >= 0

def meetsPrecedence(op, assoc, prec):
    if assoc == "none" and op.assoc == "none":
        parseError("ambiguous adjacent non-associative operator precedence")
    return prec is None or precGT(op, prec)

def getOp(ops, token):
    op = None
    if isinstance(token, tuple) and token[0] == "operator":
        op = ops.get(token[1])
    return op

def arrangeOperators(ops, tokens, assoc=None, precedence=None):
    tokens = makeStream(tokens)
    accum = []
    while not tokens.empty():
        t = tokens.next()
        result = t
        op = getOp(ops, t)
        if op is not None:
            if op.fixity == "prefix":
                if aboveAppPrec(precedence) and accum:
                    tokens.put(t)
                elif tokens.empty(): # must be standalone
                    if precedence is not None or accum:
                        parseError("prefix operator has no rhs")
                else:
                    rhs = makeExpr(arrangeOperators(ops, tokens, op.assoc,
                                                    op.prec))
                    result = makeExpr([t, rhs])
            elif precedence is not None and not accum:
                parseError("ambiguous adjacent operators")                
            elif meetsPrecedence(op, assoc, precedence):
                if op.fixity == "postfix":
                    if not accum:
                        if not tokens.empty():
                            parseError("postfix operator has no lhs")
                    else:
                        lhs = accum.pop()
                        result = makeExpr([t, lhs])
                else: # infix
                    if not accum:
                        if not tokens.empty():
                            rhs = makeExpr(arrangeOperators(ops, tokens,
                                                            op.assoc, op.prec))
                            if not tokens.empty():
                                parseError("infix operator has no lhs")
                            result = makeExpr([symbol("flip"), t, rhs])
                    else:
                        if op.prec < 0: # entire left-hand app is the lhs
                            lhs = makeExpr(accum)
                            accum = []
                        else:
                            lhs = accum.pop()
                        if tokens.empty(): # lhs section
                            if precedence is not None or accum:
                                parseError("infix operator has no rhs")
                            result = makeExpr([t, lhs])
                        else:
                            rhs = makeExpr(arrangeOperators(ops, tokens,
                                                            op.assoc, op.prec))
                            result = makeExpr([t, lhs, rhs])
            else:
                tokens.put(t)
                break
        elif aboveAppPrec(precedence) and accum:
            tokens.put(t)
            break
        accum.append(result)
    return accum

################################################################

tokenConverters = {"operator": symbol, "ident": symbol, "literal": lambda x: x}

def convertTokens(tokens):
    for t in tokens:
        if isinstance(t, tuple):
            token, value = t
            t = tokenConverters[token](value)
        yield t

def makeExpr(tokens):
    return minimalExpr(pairList(convertTokens(tokens)))

def listExpr(xs):
    p = null
    for x in reversed(list(convertTokens(xs))):
        p = pair(symbol(':'), pair(x, pair(p, null))) # magic symbol... a hack
    return p

# these should use a common function for arrangement: binary op, comma, etc.
# and then build a final linked-list form
def makeApp(ops, tokens):
    return makeExpr(arrangeOperators(ops, tokens))
def makeList(ops, tokens): return listExpr(arrangeOperators(ops, tokens))
def makeTuple(ops, tokens): return list(tokens)

brackets = {
    '(':(')', makeApp),
    '[':(']', makeList),
#    '{':('}', makeTuple)
    }

closeBrackets = set([')', ']'])#, '}'])

def innerBracketedExpr(ops, tokens, openBracket):
    handler = brackets.get(openBracket)
    if handler is None:
        if openBracket in closeBrackets:
            parseError("unmatched %s" % openBracket)
        else:
            parseError("invalid syntax %s" % openBracket)
    closeBracket, processExpr = handler
    return processExpr(ops, bracketedExpr(ops, tokens, closeBracket))

def bracketedExpr(ops, tokens, closeBracket):
    for t in tokens:
        if t[0] == "syntax":
            s = t[1]
            if s == closeBracket:
                return
            yield innerBracketedExpr(ops, tokens, s)
        else:
            yield t
    assert False, "unexpected eof"

def indentedExpr(ops, tokens, indent=-1):
    tokens = makeStream(tokens)
    for t in tokens:
        if t[0] == "syntax":
            s = t[1]
            if isinstance(s, int):
                if s <= indent:
                    tokens.put(t) # parent has to try to handle this indent too
                    return
                else:
                    yield makeApp(ops, indentedExpr(ops, tokens, s))
            else:
                yield innerBracketedExpr(ops, unindented(tokens), s)
        else:
            yield t

def parse(tokens, env, ops):
    global lineNum
    lineNum = 1
    for expr in indentedExpr(ops, indented(tokens)):
        expr = buildExpr(expr, env)
        if expr is not null:
            yield expr

def parseStream(stream, env, ops):
    return parse(streamTokens(stream), env, ops)

def parseProg(env, ops):
    return [expr for expr in parseStream(open('prog.scm'), env, ops)]
#        print expr
#     for expr in parse(streamTokens(stream)):
#         print expr

#     for expr in grouped(streamTokens(stream)):
#         print buildExpr(expr)
#         printPairList(expr)
#         print
#        print expr

# todo: binary operator grouping, (op) grabs op singularly, $ident makes an op?

"""
f x y z = (x + y)*z

g x y =
  f 2*(neg x)
    3*y, ; comma to allow neg z to be a single unit? some ambiguity
    neg z


a b c, d e f,
  g h, i j,
    k l, m n o
equiv to:
(a b c (d e f) (g h) (i j) (k l) (m n o))

for each token
  yield it if not syntax
  otherwise if it's an endbracket:
    apply end condition to token stream, possibly adding syntax (comma, indent)
    return
  otherwise if it begins a new bracketed expr
    yield bracketed expr

[1 2 3 4]
[1,2,3,4]

parse(open('prog.scm'))

"""
