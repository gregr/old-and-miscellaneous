class Symbol(object):
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return self.name
    def __repr__(self):
        return "Symbol(%r)" % self.name

def isSymbol(v):
    return isinstance(v, Symbol)

def symbol(name, syms={}):
    s = syms.get(name)
    if s is None:
        s = Symbol(name)
        syms[name] = s
    return s

class Unique(object):
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return "<unique: %s>" % self.name

def isUnique(v):
    return isinstance(v, Unique)

null = Unique("()")
true = Unique("true")
false = Unique("false")

def make_tuple(length):
    if length == 0:
        return null
    return [null]*length

def isTuple(v):
    return isinstance(v, list)

def tuple_length(a):
    return len(a)

def tuple_get(a, i):
    return a[i]

def tuple_set(a, i, v):
    a[i] = v

def tuple_(*args): # shortcut for use in interpreter
    return list(args)

def make_typed(t, v):
    return tuple_(t, v)

def isType(t, v):
    if isTuple(v):
        return t == typed_tag(v)
    return False

def typed_tag(t):
    return tuple_get(t, 0)

def typed_value(t):
    return tuple_get(t, 1)

pairTag = Unique("pair")

def pair(x, y):
    return make_typed(pairTag, tuple_(x, y))

def isPair(v):
    return isType(pairTag, v)

def pair_head(p):
    return tuple_get(typed_value(p), 0)

def pair_tail(p):
    return tuple_get(typed_value(p), 1)

appTag = Unique("app")

def app(f, x):
    return make_typed(appTag, tuple_(f, x))

def isApp(v):
    return isType(appTag, v)

def app_proc(a):
    return tuple_get(a, 0)

def app_arg(a):
    return tuple_get(a, 1)

primTag = Unique("prim")

def makePrim(p):
    return make_typed(primTag, p)

def isPrim(v):
    return isType(primTag, v)

macroTag = Unique("macro")

def makeMacro(m):
    return make_typed(macroTag, m)

def isMacro(v):
    return isType(macroTag, v)
