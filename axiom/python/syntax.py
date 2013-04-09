import lex
from data import *
from itertools import ifilter, chain

class ParseError(StandardError): pass # python already claimed SyntaxError

def unindented(ts): return ifilter((lambda t: t[0][0] != 'indentation'), ts)

def parseError(msg, attr): raise ParseError, (msg, attr)

literalConverters = dict(
    int=litInt,
    float=litFloat,
    char=litChar,
    string=litString,
    )

def makeAtom(opsTable, t, v, attr):
    if t == 'ident': return symbol(v), attr
    elif t == 'operator': return makeOperator(opsTable, symbol(v), attr), attr
    elif t == 'literal':
        tt, vv = v
        converter = literalConverters.get(tt)
        if converter is None: parseError("unknown literal type '%s'"%tt, attr)
        return converter(vv), attr
    elif t == 'meta': pass # todo
    else: parseError('invalid atom %s'%[t,v], attr)

def makeApp(terms, attr):
#    if len(terms) == 1 and isList(terms[0][0]): return terms[0]
    try:
     tas = zip(*[[t,a] for t,a in terms])
    except ValueError:
        print 'crap', terms
        raise
    if not tas: tas = ([], [])
    return toList(tas[0]), toList(tas[1])

def maybeMakeApp((terms, hasOp), attr=None):
    terms = list(terms)
    if attr is None:
        assert len(terms) > 0
        attr = terms[0][1]
    if len(terms) == 1 and hasOp: return terms[0]
    return makeApp(terms, attr)
#     if len(terms) != 1: return makeApp(terms, attr)
#     return terms[0]

def makeMacroApp(name):
    def _makeMacroApp((terms, hasOp), attr):
        return makeApp(list(chain([(symbol(name), attr)], terms)), attr)
    return _makeMacroApp

brackets = {
    '(': (')', maybeMakeApp),
    '[': (']', makeMacroApp('list')),
    '{': ('}', makeMacroApp('tuple')),
#     '#(': (')',),
#     '#[': (']',),
#     '#{': ('}',),
    }

closeBrackets = set(list(')]}'))

def deepFromList(x):
    if x is nil or x[0] is listConsTag:
        return [deepFromList(y) for y in fromList(x)]
    return x

def _test(s):
    from StringIO import StringIO
    parser = Parser()
    ops = (
        ('$', 'prefix', False, 5),
        ('!', 'prefix', False, 5),
        ('.', 'infixTight', False, 10),
        ('+', 'infix', False, 5),
        ('-', 'infix', False, 5),
        ('*', 'infix', False, 6),
        ('->', 'infix', True, 3),
        ('=', 'infix', True, 2),
        )
    for op in ops:
        opName = symbol(op[0])
        parser.opsTable.extend(EnvName(opName), newOperator(opName, *op[1:]))
    for e in parser.parse(StringIO(s)):
        print pretty(e[0])
        print deepFromList(e[1])

#_test('hello world\n  4+ 3\n\n  5 - 6\n  \nf 7-8+9 -10\n## comments\n\n')
#_test('hello world\n  4+ 3')
#_test('hello world\n  4+ ! 3 *5 + $7 + !')
#_test('hello world.tour\n  4+ ! 3 *5 + $7 + !')
#_test('hello world.tour\n  - 4+ ! 3 *5 + $7 + !')
#_test('quote (1 . 2)')

def newOperator(name, fixity, rightAssoc, prec):
    return fixities[fixity](name, rightAssoc, prec)

def makeAtom_(*args):
    try: return makeAtom(*args)
    except ParseError: return None, None
def isInfixOp(atom): return isinstance(atom[0], (InfixOp, InfixTightOp))

class Parser(object):
    def __init__(self, opsTable=None):
        if opsTable is None: opsTable = Env()
        self.opsTable = opsTable
    def parse(self, stream): return self.exprs(lex.tokenize(stream))
    def exprs(self, ts):
        ts = makeStream(iter(ts))
        for (t, indent), attr in ts:
            assert t == 'indentation', (t, attr)
            if indent < 0: break
            yield self.indentedExpr(indent, attr, ts)
    def parseOps(self, ts):
        ts = makeStream(ts)
        rhs = []
        hasOp = False
        for term in ts:
            t, a = term
            if isinstance(t, Operator):
                hasOp = True
                rhs = t.parse(rhs, a, ts)
            else: rhs.append(term)
        return rhs, hasOp
    def indentedExpr(self, i, a, ts):
        return maybeMakeApp((self.parseOps(self.indentedTerms(i,a,ts))[0],
                             True), a)
    def indentedTerms(self, indent, firstAttr, ts):
        subIndent = None
        for token in ts:
            (t,v), attr = token
            if t == 'indentation':
                if v <= indent:
                    ts.put(token) # parent has to handle this indent too
                    return
                else:
                    if subIndent is None: subIndent = v
                    peek = ts.next()
                    (pt, pv), pattr = peek
                    atom = makeAtom_(self.opsTable, pt, pv, pattr)
                    if isInfixOp(atom): yield atom
                    else:
                        #print 'peek:', peek[0]
                        ts.put(peek)
                        if subIndent > v:
                            parseError('misaligned indentation; expected '+
                                       '%d or %d but found %d' %
                                       (subIndent, indent, v),
                                       attr)
                    yield self.indentedExpr(v, attr, ts)
            elif t == 'syntax':
                yield self.bracketedExpr(v, attr, ts.compose(unindented))
            else: yield makeAtom(self.opsTable, t, v, attr)
        parseError('unexpected eof while parsing indented expr', firstAttr)
    def bracketedExpr(self, openBracket, attr, ts):
        handler = brackets.get(openBracket)
        if handler is None:
            if openBracket in closeBrackets:
                parseError('unmatched %s'%openBracket, attr)
            else: parseError('invalid syntax %s'%openBracket, attr)
        closeBracket, makeExpr = handler
        return makeExpr(self.parseOps(self.bracketedTerms(closeBracket,
                                                          attr, ts)),
                        attr)
    def bracketedTerms(self, closeBracket, firstAttr, ts):
        for token in ts:
            (t,v), attr = token
            if t == 'syntax':
                if v == closeBracket: return
                yield self.bracketedExpr(v, attr, ts)
            else: yield makeAtom(self.opsTable, t, v, attr)
        parseError('unexpected eof while parsing bracketed expr', firstAttr)

def makeOperator(opsTable, name, attr):
    op = opsTable.get(EnvName(name))
    if op is None: op = NullOp(name, False, 0)
    return op

class Operator(object):
    def __init__(self, sym, assocRight, prec):
        assert isSymbol(sym), sym
        assert type(prec) is int, prec
        self.sym = sym
        self.assocRight = assocRight
        self.prec = prec
    def parse(self, lhs, attr, ts): abstract

class NullOp(Operator): # undeclared op
    def parse(self, lhs, attr, ts):
        if not lhs and ts.empty(): return [(self.sym, attr)]
        else: parseError("unknown operator '%s'"%prettySymbol(self.sym), attr)

class PrefixOp(Operator):
    def parse(self, lhs, attr, ts):
        if ts.empty(): return lhs+[(self.sym, attr)] # slice
        t, a = ts.next()
        rhs = [(t,a)]
#         rhs = ts.next()
#         t, a = rhs
        if isinstance(t, Operator):
            if isinstance(t, PrefixOp): rhs = t.parse([], a, ts)
            else: parseError('unexpected operator while parsing prefix op', a)
        return lhs+[makeApp([(self.sym, attr)]+rhs, attr)]

def makeReducedApp(terms): return maybeMakeApp((terms, True))

def makeInfixApp(sym, lhs, rhs, attr):
    if lhs:
        if rhs: return [makeApp([(sym, attr), makeReducedApp(lhs),
                                 makeReducedApp(rhs)], attr)]
        else: return [makeApp([(sym, attr), makeReducedApp(lhs)], attr)]
    elif rhs: return [makeApp([(symbol('__slice_rhs'), attr), (sym, attr),
                               makeReducedApp(rhs)], attr)] # todo: macro names?
    else: return [(sym, attr)]

class InfixOp(Operator):
    def parse(self, lhs, attr, ts):
        if ts.empty(): return makeInfixApp(self.sym, lhs, [], attr)
        t, a = ts.next()
        rhs = [(t,a)]
        if isinstance(t, Operator):
            if isinstance(t, PrefixOp): rhs = t.parse([], a, ts)
            else: parseError('unexpected operator while parsing infix op', a)
        for term in ts:
            t, a = term
            if isinstance(t, Operator):
                if self.precLT(t): rhs = t.parse(rhs, a, ts)
                else:
                    ts.put(term)
                    return makeInfixApp(self.sym, lhs, rhs, attr)
            else: rhs.append(term)
        return makeInfixApp(self.sym, lhs, rhs, attr)
    def precLT(self, op): return (isinstance(op, (PrefixOp, InfixTightOp))
                                  or (op.prec > self.prec) or
                                  ((op.prec == self.prec) and self.assocRight))

class InfixTightOp(Operator):
    def parse(self, lhs, attr, ts):
        if lhs:
            rest = lhs[:-1]
            lhs = [lhs[-1]]
        else: rest = []
        if ts.empty(): return rest + makeInfixApp(self.sym, lhs, [], attr)
        t, a = ts.next()
        rhs = [(t,a)]
        if isinstance(t, Operator):
            if isinstance(t, PrefixOp): rhs = t.parse([], a, ts)
            else: parseError('unexpected operator while parsing infix op', a)
        if not ts.empty():
            term = ts.next()
            t, a = term
            if isinstance(t, InfixTightOp) and self.precLT(t):
                rhs = t.parse(rhs, a, ts)
            else: ts.put(term)
        return rest + makeInfixApp(self.sym, lhs, rhs, attr)
    def precLT(self, op): return ((op.prec > self.prec) or
                                  ((op.prec == self.prec) and self.assocRight))

fixities = dict(prefix=PrefixOp, infixTight=InfixTightOp, infix=InfixOp)
