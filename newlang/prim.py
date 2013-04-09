from core import (buildExpr, strictValue, lazyValue, actualValue, analyze,
                  LazyProc, StrictProc, Environment, head, tail)
from data import (symbol, isSymbol, null, true, false,
                  isPair, pair, pair_head, pair_tail, makeMacro, makePrim)
from functools import partial
import pretty

# def head_set(p, v):
#     tuple_set(p, 0, v)

# def tail_set(p, v):
#     tuple_set(p, 1, v)

# actual primitive ops to put in root env should follow

def _macroArgs(pl, n):
    pl = actualValue(pl)
    for i in xrange(n):
        assert isPair(pl), "not enough macro args: %d out of %d" % (i, n)
        yield head(pl)
        pl = tail(pl)
    assert pl is null, "too many macro args"

def macroArgs(pl, n): return tuple(_macroArgs(pl, n))

def _makeAssign(assignMethod, evalFunc):
    @makeMacro
    def _assigner(args, _):
        param = macroArgs(args, 1)[0]
        assert isSymbol(param), assignMethod+" param is not symbol: "+str(param)
        @makePrim
        def setupAssign(e):
            def doAssign(arg, env):
                getattr(e, assignMethod)(param, evalFunc(arg, env))
                return null, None
            return doAssign, None
        return setupAssign, _
    return _assigner

_makeDefine = partial(_makeAssign, "define")
_makeSet = partial(_makeAssign, "set")

def _makeProc(procFunc):
    @makeMacro
    def _proc(args, env):
        param, body = macroArgs(args, 2)
        assert isSymbol(param), "proc param must be a symbol"
        body = analyze(buildExpr(body, env))
        return makePrim(lambda e: (procFunc(param, body, e), None)), env
    return _proc

@makeMacro
def _macro(args, env):
    args, body = macroArgs(args, 2) # param takes a whole list?
    body = analyze(buildExpr(body, env))
    @makeMacro
    def expand(cargs, cenv):
        cargs = actualValue(cargs)
        params = args
        bindings = {}
#         import pretty
#         print pretty.pretty(params)
#         print pretty.pretty(cargs)
        while params is not null:
            param = head(params)
            assert isSymbol(param), "macro param must be a symbol"
            params = tail(params)
            if params is null:
                bindings[param] = cargs
            else:
                bindings[param] = head(cargs)
                cargs = tail(cargs)
        menv = Environment(cenv, bindings)
        result = strictValue(body, menv)
#        print pretty.pretty(result)
        return result, cenv
    return expand, env

def finalOp(op):
    def inner(*args):
        return op(*args), None
    return inner

def makeCurryOp(evalFunc):
    def curryOp(op, nargs):
        def outer(arg, env):
            def inner(args, arg, env):
                args = args[:]
                args.append(evalFunc(arg, env))
                if len(args) < nargs:
                    return partial(inner, args), None
                return op(*args)
            return inner([], arg, env)
        return outer
    return curryOp

curryOp = makeCurryOp(strictValue)
curryOpLazy = makeCurryOp(lazyValue)

def ifOp(cond, env):
    cond = strictValue(cond, env)
    if cond is true:
        def ifResult(arg, env):
            return (lambda _a, _e: (arg, env), None)
    elif cond is false:
        def ifResult(_a, _e):
            return (lambda arg, env: (arg, env), None)
    else:
        assert False, "cond must be either true or false"
    return ifResult, None

@makeMacro
def quote(args, _):
    arg = macroArgs(args, 1)[0]
    return makePrim(lambda _: (arg, None)), _

# def pairOp(headArg, env):
#     headArg = lazyValue(headArg, env)
#     def pairOpTail(tailArg, env):
#         tailArg = lazyValue(tailArg, env)
#         return pair(headArg, tailArg)
#     return pairOpTail

def boolean(f):
    def translateBoolean(*args):
        if f(*args):
            return true
        return false
    return translateBoolean

# todo: move everything that isn't axiomatic to the prelude
@boolean
def is2(x, y): return x is y
# @boolean
# def and2(x, y): return (x is not false) and (y is not false)
# @boolean
# def or2(x, y): return (x is not false) or (y is not false)
@boolean
def eq2(x, y): return x == y
@boolean
def neq2(x, y): return x != y
@boolean
def gt2(x, y): return x > y
@boolean
def gte2(x, y): return x >= y
@boolean
def lt2(x, y): return x < y
@boolean
def lte2(x, y): return x <= y

# def not1(x):
#     if x is false:
#         return true
#     return false

def add2(x, y): return x + y
def sub2(x, y): return x - y
def mul2(x, y): return x * y
def div2(x, y): return x / y
def mod2(x, y): return x % y
def pow2(x, y): return x ** y

binops = (
    (is2, "is"),
#     (and2, "and"),
#     (or2, "or"),
    (eq2, "eq"),
    (neq2, "neq"),
    (gt2, "gt"),
    (gte2, "gte"),
    (lt2, "lt"),
    (lte2, "lte"),
    (add2, "add"),
    (sub2, "sub"),
    (mul2, "mul"),
    (div2, "div"),
    (mod2, "mod"),
    (pow2, "pow"),
    )

def prettyPrint(x):
    print pretty.pretty(x)
    return null

strictOps = (
#    (not1, "not", 1),
    (boolean(isPair), "isPair", 1),
    (pair_head, "head", 1),
    (pair_tail, "tail", 1),
    (prettyPrint, "print", 1),
)

lazyOps = (
    (pair, "pair", 2),
    (pair, ":", 2),
)

root = {
    "true": true,
    "false": false,
    "quote": quote,
    "if": ifOp,
    "lazyDef": _makeDefine(lazyValue),
    "strictDef": _makeDefine(strictValue),
    "lazySet": _makeSet(lazyValue),
    "strictSet": _makeSet(strictValue),
    "lazyProc": _makeProc(LazyProc),
    "strictProc": _makeProc(StrictProc),
    "macro": _macro,
    }

def addRootOps(ops, curryOpType):
    for op, sym, count in ops:
        root[sym] = curryOpType(finalOp(op), count)

for op, sym in binops:
    root[sym] = curryOp(finalOp(op), 2)

addRootOps(strictOps, curryOp)
addRootOps(lazyOps, curryOpLazy)

rootSyms = dict((symbol(sym), op) for sym, op in root.iteritems())
rootEnv = Environment(None, rootSyms)
