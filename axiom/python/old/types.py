from itertools import cycle
from data import *

class TypeEnvVal(object):
    def __init__(self, t=None, deps):
        self.t = t
        self.deps = deps

#def tenv_get(tenv, n): return tenv.get(n).t
def tenv_getDeps(tenv, n): return tenv.get(n).deps

class Type(Named):
    def teval(self, tenv): undefined

class TypeLit(Type):
    def teval(self, tenv): return self, False, None
    def tvars(self): return set()

class TypeVar(Type):
    def teval(self, tenv):
        t = tenv.get(self.n)
        if t is None:
            return None, False, None
        return t.t, True, t.deps
    def tvars(self): return set((self,))
    def tconstrain(self, tenv, rhs):
        if self.isMentioned(rhs): infinite type
    def isMentioned(self, rhs): pass

class TypeCons(Type):
    def __init__(self, tag, args):
        Type.__init__('{%s %s}' % (tag, ' '.join(args)))
        self.tag = tag
        self.args = args
    def teval(self, tenv):
        reduced = False
        args = []
        deps = []
        for arg in self.args:
            a, r, ds = arg.teval(tenv)
            args.append(a)
            deps.extend(ds)
            reduced = r or reduced
        if reduced: return TypeCons(self.tag, args), True, deps
        else: return self, False, None
    def tvars(self):
        vs = set()
        for arg in self.args: vs |= arg.tvars()
        return vs
#class TypeSet(Type): pass

class TypeDep(Type): # type thunk
    def __init__(self, expr, cenv, deps):
        Type.__init__('(%s, %s, %s)' % (expr, cenv, deps))
        self.expr = expr
        self.cenv = cenv
        self.deps = deps
        self.solution = None
    def teval(self, tenv):
        if self.solution is None:
            self.solution = self.expr.ceval(self.cenv)
        return self.solution

arrow = symbol('->')
tup = symbol('(,)')

class Expr(Named):
    def ceval(self, cenv): undefined

class ExprLit(Expr):
    pyTypeToTypeLit = {int: TypeLit('Int'), str: TypeLit('String')}
    def __init__(self, v):
        Expr.__init__(repr(v))
        self.v = v
    def ceval(self, cenv): return self.pyTypeToTypeLit[type(self.v)]

class ExprVar(Expr):
    def ceval(self, cenv):
        t = cenv.get(self.n)
        if t is None: return TypeDep(self, cenv, self.n)
        else return t

class ExprApp(Expr):
    def __init__(self, f, x):
        Expr.__init__('(%s %s)' % (f, x))
        self.f = f
        self.x = x
    def ceval(self, cenv): # constrain with self.f
        ft = self.f.ceval(cenv)
        xt = self.x.ceval(cenv)
        deps = []
        if isinstance(ft, TypeDep): deps.extend(ft.deps)
        if isinstance(xt, TypeDep): deps.extend(xt.deps)
        if deps: return TypeDep(self, cenv, deps)
        rt = cenv.tenv.nextVar()
        ft.tconstrain(cenv.tenv, TypeCons(arrow, [xt, rt]))
        return rt

class ExprTuple(Expr):
    def __init__(self, args):
        Expr.__init__(str(args))
        self.args = args
    def ceval(self, cenv):
        deps = []
        args = []
        for arg in self.args:
            at = arg.ceval(cenv)
            args.append(at)
            if isinstance(at, TypeDep): deps.extend(at.deps)
        if deps: return TypeDep(self, cenv, deps)
        return TypeCons((tup, len(args)), args)

class ExprCase(Expr): pass

class ExprProc(Expr): pass
