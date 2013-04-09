from data import (null, app, app_proc, app_arg, isApp, typed_value, isSymbol,
                  symbol, pair, isPair, pair_head, pair_tail, isMacro, isPrim)

class Delayed(object):
    def __init__(self, value, env):
        self.value = value
        self.env = env
    def eval(self):
        if self.env is not None:
            self.value = strictValue(self.value, self.env)
            self.env = None
        return self.value

def isDelayed(x):
    return isinstance(x, Delayed)

def actualValue(val):
    if isDelayed(val):
        return val.eval()
    return val

def lazyValue(ex, env):
    if isinstance(ex, Thunk):
        return ex.lazyValue(env)
    return ex

# todo: errors
class Environment(object):
    def __init__(self, parent, bindings=None):
        self.parent = parent
        if bindings is None:
            bindings = {}
        self.bindings = bindings
    def get(self, sym):
        val = self.bindings.get(sym)
        if val is None:
            if self.parent is not None:
                return self.parent.get(sym)
        return val
    def set(self, sym, val):
        if not self._set(sym, val):
            # CoreError would be caught and returned due to execute... hmm
            raise StandardError, "assigning to undefined variable: %s" % sym
#            self.define(sym, val)
    def _set(self, sym, val):
        if self.bindings.has_key(sym):
            self.bindings[sym] = val
            return True
        elif self.parent is not None:
            return self.parent._set(sym, val)
        return False
    def define(self, sym, val):
        self.bindings[sym] = val
    def __repr__(self):
        return "<Environment: %s>" % map(str, self.bindings.keys())

# todo: catching exceptions/signals?
class CoreError(StandardError): pass

def execute(ex, env):
    try:
        while env is not None:
            ex, env = ex(env)
    except CoreError, e:
        ex = e
    return ex

def strictValue(ex, env):
    if isinstance(ex, Thunk):
        ex = execute(ex, env)
    return actualValue(ex)

isVariable = isSymbol

def isAtom(expr):
    return not isApp(expr) and not isVariable(expr)

class Thunk(object): pass

class AtomThunk(Thunk):
    def __init__(self, atom):
        self.atom = atom
    def __call__(self, env):
        return self.atom, None
    def lazyValue(self, env):
        return self.atom

class VariableThunk(Thunk):
    def __init__(self, var):
        self.var = var
    def __call__(self, env):
        return self.lazyValue(env), None
    def lazyValue(self, env):
        val = env.get(self.var)
        assert val is not None, "undefined variable: %s" % self.var
        return val

class AppThunk(Thunk):
    def __init__(self, expr):
        a = typed_value(expr)
        self.proc = analyze(app_proc(a))
        self.arg = analyze(app_arg(a))
    def __call__(self, env):
        pc = strictValue(self.proc, env)
        return pc(self.arg, env)
    def lazyValue(self, env):
        return Delayed(self, env)
#         return Delayed(AppInstanceThunk(lazyValue(self.proc, env),
#                                      lazyValue(self.arg, env)), env)
#        self.proc = lazyValue(self.proc, env)
#         print "app:", self.proc
#         print "expr:", self.expr
#        self.arg = lazyValue(self.arg, env)
#        return self

# class AppInstanceThunk(Thunk):
#     def __init__(self, proc, arg):
#         self.proc = proc
#         self.arg = arg
#     def __call__(self, env):
#         return strictValue(self.proc, env)(self.arg, env)
#     def lazyValue(self, env):
#         return self

class PrimThunk(Thunk):
    def __init__(self, prim):
        self.prim = typed_value(prim)
    def __call__(self, env):
        return self.prim(env)
    def lazyValue(self, env):
        return Delayed(self, env)

def analyze(expr): #, cenv): # compile-time environment?
    if isVariable(expr):
        return VariableThunk(expr)
    elif isApp(expr):
        return AppThunk(expr)
    elif isPrim(expr):
        return PrimThunk(expr)
    else: # atom
        return AtomThunk(expr)

class Proc(object):
    eval = None
    def __init__(self, param, body, env):
        self.env = env
        self.param = param
        self.body = body
    def __call__(self, arg, env):
        e = Environment(self.env, {self.param: self.eval(arg, env)})
        return self.body, e # for tail calls

def isProc(v):
    return isinstance(v, Proc)

class LazyProc(Proc):
    eval = staticmethod(lazyValue)
class StrictProc(Proc):
    eval = staticmethod(strictValue)

def head(p): return actualValue(pair_head(p))
def tail(p): return actualValue(pair_tail(p))

def pairListIter(p):
#    p = actualValue(p)
    while p is not null:
        yield pair_head(p)
        p = tail(p)

def pairIter(p):
#    p = actualValue(p)
    while p is not null:
        if not isPair(p):
            yield p
            break
        yield pair_head(p)
        p = tail(p)

def pairList(xs):
    p = null
    for x in reversed(list(xs)):
        p = pair(x, p)
    return p

def minimalExpr(p):
    if p is not null and tail(p) is null:
        return head(p)
    return p

def buildExpr(p, env):
    while True: # for tail calling during macro expansion
        p = actualValue(p)
        if not isPair(p):
            return p
        expr = buildExpr(pair_head(p), env)
        m = expr # lookup possible macro
        if isSymbol(m):
            m = actualValue(env.get(m))
        if isMacro(m): # if it is, expand
            m = typed_value(m)
            p, env = m(pair_tail(p), env)
            continue
        p = tail(p)
        for x in pairListIter(p):
            expr = app(expr, buildExpr(x, env))
#        print expr
        return expr

class Operator(object):
    def __init__(self, fixity, assoc, prec):
        assert fixity in ("prefix", "infix", "postfix"), fixity
        assert assoc in ("left", "none", "right"), assoc
        self.fixity = fixity
        self.assoc = assoc 
        self.prec = prec
