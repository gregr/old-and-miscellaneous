from module import Module
from data import *
from itertools import izip

class Tracer(object): # subclassed for debugging, profiling, etc.
    def backtrace(self): pass
    def push(self): return self
    def pop(self, subTracer): pass
    def add(self, expr, env): pass

class EvalError(StandardError): pass

def evalError(msg, attr): raise EvalError, (msg, attr)

def evalExpr(expr, env, tracer): # tail-call trampoline
    subTracer = tracer.push()
    while env is not None:
        subTracer.add(expr, env)
        try: expr, env = expr.eval(env, subTracer)
        except:
            subTracer.backtrace()
            raise
    subTracer.add(expr, env)
    tracer.pop(subTracer)
    return expr # expr is no longer really an expr here

def flatten(xss):
    acc = []
    for xs in xss: acc.extend(xs)
    return acc

class Expr(object):
    def __init__(self, attr): self.attr = attr
    def eval(self, env, tracer): abstract
    def shouldDelay(self): return False

################################################################
class Atom(Expr): pass

class Lit(Atom):
    def __init__(self, v, *args):
        Atom.__init__(self, *args)
        self.v = v
    def __repr__(self): return '<Lit %r>'%self.v
    def eval(self, env, tracer): return self.v, None
    def shouldDelay(self): return self.v is not undefined # todo: necessary?
    def teval(self, tenv): pass # todo: python-val -> unboxed type

def getTypeVar(name, tenv):
    t = tenv.get(name)
    if t is None:
        t = TypeVar() # new var
        tenv.extend(name, t)

class Var(Atom):
    def __init__(self, name, *args):
        Atom.__init__(self, *args)
        self.name = name
    def __repr__(self): return '<Var %r>'%self.name
    def _eval(self, env): return env.get(self.name)
    def eval(self, env, tracer):
        v = self._eval(env)
        if v is None:
            evalError("unbound variable '%s' in env: %s"%(self.name, env),
                      self.attr)
        return v, None # todo: handle None
    def shouldDelay(self): return True # todo: static analysis can do better
    def teval(self, tenv): return getTypeVar(self.name, tenv)

################################################################
class Data(Expr): pass

class TypeTagged(Data):
    def __init__(self, arg, *args):
        Data.__init__(self, *args)
        self.arg = arg
    def __repr__(self): return '<TypeTagged %r>'%self.arg
    def shouldDelay(self): return self.arg.shouldDelay()
    def eval(self, env, tracer):
        v = evalExpr(self.arg, env, tracer)
        return node(node_tag(v).tyTag, v), None

class Construct(Data):
    def __init__(self, tag, cargs, *args):
        Data.__init__(self, *args)
        self.tag = tag
        self.args = cargs
    def __repr__(self): return '<Construct %r>'%([self.tag]+self.args)
    def eval(self, env, tracer):
        return node(self.tag,
                    *[evalExpr(arg, env, tracer) for arg in self.args]), None
    def shouldDelay(self): return any(arg.shouldDelay() for arg in self.args)
    def teval(self, tenv):
        args = [arg.teval(tenv) for arg in self.args]
        locEnv = Env()
        for targ, arg in izip(self.tag.argTypes, args):
            if type(targ) is TypeSetVar:
                getTypeVar(targ.envName, locEnv).require(arg) # envName...
            elif type(targ) is TypeLitVar: #todo...
                t = getTypeVar(targ.envName, locEnv)
                t.constrain(arg)
                getTypeVar(targ.setName, locEnv).require(t)
        #return ProductType()
        return self.tag.typeCons(args)

class ConsRecord(Data):
    names = nameGen(['R__'])
    def __init__(self, bindings, name, *args):
        Data.__init__(self, *args)
        self.bindings = bindings
        name = name or names.next()
        self.tag = RecordTag(name)
    def __repr__(self): return '<ConsRecord %r>'%self.tag
    def eval(self, env, tracer):
        recEnv = Env()
        for bname, bval in self.bindings:
            recEnv.extend(EnvName(bname), evalExpr(bval, env, tracer))
        return node(self.tag, recEnv), None
    def shouldDelay(self): return True

class Abstract(Data):
    names = nameGen(['F__'])
    def __init__(self, binder, body, name, *args):
        Data.__init__(self, *args)
        self.binder = binder # valid env name
        self.body = body
        if name is None: name = names.next()
        self.tag = procTag(name)
    def __repr__(self): return '<Abstract %r>'%self.tag
    def eval(self, env, tracer):
        return proc_new(self.tag, self.binder, self.body, env), None
    def teval(self, tenv):
        return newProcType(getTypeVar(self.binder,tenv), self.body.teval(tenv))

################################################################
class Accessor(Expr):
    def __init__(self, node, *args):
        Expr.__init__(self, *args)
        self.node = node
    def eval(self, env, tracer):
        try: return self._eval(env, tracer), None
        except:
            print 'ad-hoc traceback (to be improved):', self.attr
            print
        raise
    def shouldDelay(self): return self.node.shouldDelay()

class NodeAccessor(Accessor):
    def __init__(self, index, tag, *args):
        Accessor.__init__(self, *args)
        assert index < len(tag.argTypes), (index, len(tag.argTypes))
        self.index = index+1 # index 0 holds the tag
        self.tag = tag
    def __repr__(self): return '<%s %r %r>'%(self.__class__.__name__,
                                             self.tag, self.index)
    def _evalNode(self, env, tracer):
        node = evalExpr(self.node, env, tracer)
        assert node_tag(node) == self.tag, (node_tag(node), self.tag)
        return node

# used internally by letrec and case for irrefutable/lazy patterns
class Unpack(NodeAccessor):
    def _eval(self, env, tracer):
        return self._evalNode(env,tracer)[self.index]

class Pack(NodeAccessor): # todo: only use with mutable fields...
    def __init__(self, rhs, *args):
        NodeAccessor.__init__(self, *args)
        self.rhs = rhs
    def _eval(self, env, tracer):
        rhs = evalExpr(self.rhs, env, tracer)
        self._evalNode(env, tracer)[self.index] = rhs
        return unit

class RecordAccessor(Accessor):
    def __init__(self, fname, *args):
        Accessor.__init__(self, *args)
        self.fname = fname
    def __repr__(self): return '<%s %r %r>'%(self.__class__.__name__,
                                             self.node, self.fname)
    def _evalNodeFname(self, env, tracer):
        node = evalExpr(self.node, env, tracer)
        fname = evalExpr(self.fname, env, tracer)
        return node, fname

class GetField(RecordAccessor): # todo: typing fname may be slightly difficult
    def _eval(self, env, tracer):
        node, fname = self._evalNodeFname(env, tracer)
        return node_getField(node, fname)

class SetField(RecordAccessor): # todo: only use with mutable fields
    def __init__(self, rhs, *args):
        RecordAccessor.__init__(self, *args)
        self.rhs = rhs
    def _eval(self, env, tracer):
        rhs = evalExpr(self.rhs, env, tracer)
        node, fname = self._evalNodeFname(env, tracer)
        node_setField(node, fname, rhs)
        return unit

################################################################
class Term(Expr):
    def shouldDelay(self): return True

class Delay(Term):
    def __init__(self, expr, *args):
        Term.__init__(self, *args)
        self.expr = expr
    def __repr__(self): return '<Delay %r>'%self.expr
    def eval(self, env, tracer):
        if isinstance(self.expr, Var): # todo: see note in Var.shouldDelay
            v = self.expr._eval(env, tracer)
            if v is not None: return v, None
        return thunk_new(self.expr, env), None
    def teval(self, tenv): return lazyType(self.expr.teval(tenv))

class Force(Expr):
    def __init__(self, term, *args):
        Expr.__init__(self, *args)
        self.term = term
    def __repr__(self): return '<Force %r>'%self.term
    def eval(self, env, tracer): return force(evalExpr(self.term, env,
                                                       tracer), tracer), None
    def shouldDelay(self): return self.term.shouldDelay()
    def teval(self, tenv): return strictType(self.term.teval(tenv))

# note: Delay terms must not be auto-lifted; shouldDelay must be True
# consider: force (delay (delay BOTTOM)) ## should not evaluate to BOTTOM!
def delay(expr, *args):
    if expr.shouldDelay(): return Delay(expr, *args)
    return expr # otherwise perform auto-lifting

def force(n, tracer):
    if isThunk(n): return thunk_force(n, tracer)
    return n

class Apply(Term):
    def __init__(self, proc, arg, *args):
        Term.__init__(self, *args)
        self.proc = proc
        self.arg = arg
    def __repr__(self): return '<Apply %r %r>' % (self.proc, self.arg)
    def eval(self, env, tracer):
        return proc_apply(evalExpr(self.proc, env, tracer),
                          evalExpr(self.arg, env, tracer),
                          self.attr)
    def teval(self, tenv):
        tproc = self.proc.teval(tenv)
        # todo: pass tenv to constrain for deps?
        tproc.constrain(newProcType(self.arg.teval(tenv), Any))
        return tproc.tresult # todo

class Call(Term):
    def __init__(self, prim, cargs, *args):
        Term.__init__(self, *args)
        self.prim = prim
        self.args = cargs
    def __repr__(self): return '<Call %r %r>' % (self.prim, self.args)
    def eval(self, env, tracer):
        return prim_call(evalExpr(self.prim, env, tracer),
                         [evalExpr(a, env, tracer) for a in self.args],
                         self.attr)
    def teval(self, tenv):
        tprim = self.prim.teval(tenv)
        tprim.constrain(newPrimProcType([arg.teval(tenv) for arg in self.args],
                                        Any))
        return tprim.tresult

class LetRec(Expr):
    def __init__(self, names, values, body, *args):
        Expr.__init__(self, *args)
        self.names = names
        self.values = values
        self.body = body
    def __repr__(self): return '<LetRec %r %r>' % (self.names, self.body)
    def eval(self, env, tracer):
        nenv = Env(env)
        for name in self.names: nenv.extend(name, None)
        for name, val in izip(self.names, self.values):
            nenv.extend(name, evalExpr(val, nenv, tracer))
        return self.body, nenv
    def shouldDelay(self):
        return (self.body.shouldDelay() or
                any(v.shouldDelay() for v in self.values))

class Switch(Expr): # todo: use this to replace Case
    def __init__(self, discriminant, default, alts, *args):
        Expr.__init__(self, *args)
        self.discrim = discriminant
        self.default = default
        self.alts = alts
    def __repr__(self): return '<Switch %r>' % self.discrim
    def eval(self, env, tracer):
        discrim = evalExpr(self.discrim, env, tracer)
        cont = self.alts.get(discrim)
        if cont is None: cont = self.default
        return cont, env
    def shouldDelay(self):
        return (self.discrim.shouldDelay() or
                any(p.shouldDelay() or b.shouldDelay for p,b in self.alts))

class Case(Expr): # todo: product patterns are lazy (need type-checking first)
    def __init__(self, arg, alts, *args):
        Expr.__init__(self, *args)
        self.arg = arg
        self.alts = alts
    def __repr__(self): return '<Case %r>' % self.arg
    def eval(self, env, tracer):
        arg = evalExpr(self.arg, env, tracer)
        for p, body in self.alts:
            nenv = p.match(arg, env, Env(env))
            if nenv is not None: return body, nenv
        evalError("no matching alternative for case argument '%s'"%pretty(arg),
                  self.attr)
#        assert False, (arg, self.arg, env, self.alts)
    def shouldDelay(self):
        return (self.arg.shouldDelay() or
                any(p.shouldDelay() or b.shouldDelay for p,b in self.alts))
    def teval(self, tenv):
        targ = self.arg.teval(tenv)
        ps, bs = zip(self.alts)
        targ.constrain(sumType(p.teval(tenv) for p in ps))
        return unionType(b.teval(tenv) for b in bs)

class Thrown(Exception):
    def __init__(self, exc):
        Exception.__init__(self, (exc,))
        self.exc = exc

# todo: Throw instead of __call __throw? what is throw's type?

class Catch(Expr):
    def __init__(self, arg, handler, handlerAttr, *args): # todo: arg attr for tracing
        Expr.__init__(self, *args)
        self.arg = arg
        self.handler = handler
        self.hAttr = handlerAttr
    def __repr__(self): return '<Catch %r>' % self.arg
    def eval(self, env, tracer):
        try: return evalExpr(self.arg, env, tracer), None
        except Thrown, e: # todo, test
            return Apply(self.handler, Lit(e.exc), self.hAttr).eval(env,tracer)
    def shouldDelay(self): return self.arg.shouldDelay()

################################################################
class Pattern(object):
    def match(self, val, env, nenv): abstract
    def boundNames(self): return []
class DefPattern(Pattern):
    def match(self, val, env, nenv): return env
class VarPattern(Pattern):
    def __init__(self, name): self.name = name
    def match(self, val, env, nenv):
        nenv.extend(EnvName(self.name), val)
        return nenv
    def boundNames(self): return [self.name]
class LitPattern(Pattern):
    def __init__(self, lit): self.lit = lit
    def match(self, val, env, nenv):
        if val == self.lit: return env
        return None
class CompoundPattern(Pattern):
    def __init__(self, subPatterns): self.subPatterns = subPatterns
    def match(self, val, env, nenv):
        if len(val) == len(self.subPatterns):
            for pat, v in izip(self.subPatterns, val):
                nenv = pat.match(v, nenv, nenv)
                if nenv is None: return None
            return nenv
        return None
    def boundNames(self):
        return flatten(p.boundNames() for p in self.subPatterns)

################################################################
# data
# todo: thunkTag as a type of nodeTag, then specific thunkTags for all papps
# todo: force implemented as a case analysis within the language
thunkTyTag, thunkTag = prodTagN('Thunk', 2)
def isThunk(n): return node_tag(n) is thunkTag
def thunk_new(expr, env): return node(thunkTag, expr, env)
def thunk_force(t, tracer):
    if t[2] is None: return t[1]
    v = evalExpr(t[1], t[2], tracer)
    t[1] = v
    t[2] = None
    return v

def isPrim(p): return node_tag(p).tyTag is primProcTyTag
def primTag(name): return nodeTagN(primProcTyTag, name, 1)
def prim_new(tag, prim): return node(tag, prim)
def prim_call(p, args, attr):
    if not isPrim(p):
        evalError("called value is not a primitive procedure '%s'"%p, attr)
    return p[1](*args)

def isProc(p): return node_tag(p).tyTag is procTyTag
def procTag(name): return nodeTagN(procTyTag, name, 3)
def proc_new(tag, binder, body, env): return node(tag, binder, body, env)
def proc_apply(p, arg, attr):
    if not isProc(p): evalError("applied value is not a procedure '%s'"%p,attr)
    env = Env(p[3])
    env.extend(p[1], arg)
    return p[2], env

specialTyTag, specialTag = prodTagN('__Special', 1)
def isSpecial(s): return node_tag(s) is specialTag
def special_new(prim): return node(specialTag, prim)
def special_prim(s):
    assert isSpecial(s), s
    return s[1]
def special_apply(s, arg, senv, mod, interp, tracer, isTop, isLHS, attr):
    p = special_prim(s)
    return prim_call(p, (arg, attr, senv, mod, interp, tracer, isTop, isLHS),
                     cons_head(attr))

macroTyTag, macroTag = prodTagN('__Macro', 2)
def isMacro(m): return node_tag(m) is macroTag
def macro_new(proc, senv): return node(macroTag, proc, env_new(senv))
def macro_senv(mac):
    assert isMacro(mac), mac
    return mac[2]
def macro_proc(mac):
    assert isMacro(mac), mac
    return mac[1]
def macro_apply(mac, form, attr, senv, tracer):
    p = macro_proc(mac)
    at = cons_head(attr)
    expr = evalExpr(Apply(Apply(Apply(Lit(p, at),
                                      Lit(macro_senv(mac), at), at),
                                Lit(env_new(senv), at), at),
                          Lit(form, attr), attr), Env(), tracer)
    lookup = makeAttrLookup(form, attr)
    return expr, expandAttr(expr, lookup, at)

def zipAttrFlatten(expr, attr):
    ys = [(repr(expr), attr)]
    if isList_safe(expr) and isList_safe(attr) and isListCons(expr):
        ex,at = expr,attr
        while at is not nil:
#            print 'attr:', at
            ys.append((repr(ex), at))
            if ex is nil: break
            ex,at = cons_tail(ex), cons_tail(at)
        for ex,at in izip(fromList(expr), fromList(attr)):
            ys.extend(zipAttrFlatten(ex, at))
    return ys

def makeAttrLookup(expr, attr):
    return dict(zipAttrFlatten(expr, attr))

def expandAttr(expr, lookup, defAttr):
    if isSyntax(expr): expr = syntax_form(expr)
    if isinstance(expr, Expr): return None
    attr = lookup.get(repr(expr))
    if attr is None:
        attr = defAttr
        if expr is nil: return nil
        if isListCons(expr):
            ex = expr
            rest = []
            tail = None
            while ex is not nil:
                tail = lookup.get(repr(ex))
                if tail is not None: break
                rest.append(cons_head(ex))
                ex = cons_tail(ex)
            return toList([expandAttr(ex, lookup, defAttr) for ex in rest],
                          tail)
    return attr

# def expandAttr(expr, attr):
#     if isSyntax(expr): expr = syntax_form(expr)
#     if isinstance(expr, Expr): return None
#     elif expr is nil: return nil
#     elif isListCons(expr):
#         return toList([expandAttr(e, attr) for e in fromList(expr)])
#     if attr is not None: return cons_head(attr) # todo: naive for now
#     return None
