class Env(object):
    __slots__ = ['p', 'bs']
    def __init__(self, p=None):
        self.p = p
        self.bs = {}
    def get(self, n):
        for e in self._lineage():
            v = e.bs.get(n)
            if v is not None: return v
        return None
    def getLocal(self, n): return self.bs.get(n)
    def extend(self, n, v):
        assert self.bs.get(n) is None, "redefinition of '%s'"%n
        self.bs[n] = v
    def localBindings(self): return self.bs.iteritems()
    def allBindings(self):
        bs = {}
        for e in reversed(tuple(self._lineage())): bs.update(e.bs)
        return bs
    def __repr__(self): return '<Env>'
#     def __repr__(self):
#         return '<Env '+'\n'.join(repr(e.bs) for e in self._lineage())+'>'
    def _lineage(self):
        e = self
        while e is not None:
            yield e
            e = e.p

class EnvName(object):
    __slots__ = ['sym']
    def __init__(self, sym):
        assert isSymbol(sym)
        self.sym = sym
    def __hash__(self): return symbol_id(self.sym)
    def __eq__(self, n): return hash(self) == hash(n)
    def __repr__(self): return '<EnvName %r>' % prettySymbol(self.sym)
    def __str__(self): return prettySymbol(self.sym)

class Named(object):
    __slots__=['n']
    def __init__(self, n): self.n = n
    def __repr__(self): return '<%s %s>'%(self.__class__.__name__, self.n)

class NodeTag(Named):
    def __init__(self, tyTag, n, argTypes, fieldMapping=None):
        Named.__init__(self, n)
#        print n, fieldMapping
        self.fields = fieldMapping or {}
        # todo: track mutable fields? in argTypes?
        self.argTypes = argTypes # todo: actual type exprs from data def
        self.tyTag = tyTag
    def numFields(self): return len(self.argTypes)
    def fieldSyms(self, node): # node unused
        return toList([fn.sym for fn in self.fields.iterkeys()])
    def get(self, node, fname): return node[self._index(fname)]
    def set(self, node, fname, v): node[self._index(fname)] = v
    def _index(self, fname):
        index = self.fields.get(fname)
        if index is None: typeError("data '%s' has no field named '%s'"%
                                    (self.n, fname))
        return index

def node(tag, *args):
    assert len(args) == tag.numFields(), (len(args), tag.numFields())
    return [tag]+list(args)
def node_tag(node):
    tag = node[0]
    assert isinstance(tag, NodeTag), (type(tag), tag, node)
    return tag
def node_getField(node, fname): return node[0].get(node, EnvName(fname))
def node_setField(node, fname, v): return node[0].set(node, EnvName(fname), v)
# def node_bindings(node, vs):
#     assert len(node) == len(vs), (node, vs)
#     return dict(zip(vs, node[1:]))

def nodeTag(ty, name, argTypes, fields=()):
    return NodeTag(ty, name, argTypes, dict((EnvName(symbol(fn)), i+1)
                                            for i,fn in enumerate(fields)
                                            if fn is not None))

def nodeTagN(ty, name, nFields): return NodeTag(ty, name, [None]*nFields, {})

class TypeError(StandardError): pass

def typeError(msg, attr=None): raise TypeError, (msg, attr)

class TypeTag(NodeTag):
    ty = None
    def __init__(self, n, params): # todo: actual param exprs
        #selfTy = node(self, *((None,)*nparams)) # todo: type expr
        ty = None # use params
        NodeTag.__init__(self, TypeTag.ty, n, (ty,), {})
        self.consTags = []

tyty = TypeTag('Type', [None])
tyty.tyTag = tyty
TypeTag.ty = tyty

procTyTag = TypeTag('->', ['A', 'B']) # todo: type vars
primProcTyTag = TypeTag('#->', ['(A)', 'B'])

def makeType(tname, params, consArgs):
    ty = TypeTag(tname, params)
    ty.consTags = [NodeTag(ty, *args) for args in consArgs]
    return ty

def prodTag(name, argts):
#     ty = TypeTag(name+'Ty', [])
#     return ty, nodeTag(ty, name, argts)
    ty = TypeTag(name, [])
    return ty, nodeTag(ty, 'Mk'+name, argts)
def prodTagN(name, nargs): return prodTag(name, (None,)*nargs)
def singleTag(name): return prodTag(name, ())

# todo: error?
undefTyTag, undefTag = singleTag('Undefined')
undefined = node(undefTag) # todo: is this necessary? undefined = throw x?
unitTyTag, unitTag = singleTag('Unit')
unit = node(unitTag)
################################################################
# records
def isRecord(node): return isinstance(node_tag(node), RecordTag)
def record_data(node):
    assert isRecord(node), node
    return node[1]
class RecordTag(NodeTag):
    def __init__(self, rname):
        NodeTag.__init__(self, None, rname, (None,)) # todo: tyTag?
    def fieldSyms(self, node):
        return toList([fn.sym for fn in record_data(node).bs.iterkeys()])
    def get(self, node, fname):
        env = record_data(node)
        v = env.get(fname)
        if v is None: typeError("record '%s' has no field named '%s'"%
                                (self.n, fname))
        return v
    #def set(self, node, fname, v): node[self.fields[fname]] = v

################################################################
# modules
def isModule(node): return isinstance(node_tag(node), ModuleTag)
def module_data(node):
    assert isModule(node), node
    return node[1]
class ModuleTag(NodeTag):
    def __init__(self, mname):
        NodeTag.__init__(self, None, mname, (None,))
    def fieldSyms(self, node): return toList(module_data(node).modeSyms())
    def get(self, node, fname):
        v = module_data(node).lookup(fname.sym)
        if v is None: typeError("module '%s' has no field named '%s'"%
                                (self.n, fname))
        return v

################################################################
# symbols
symTyTag, symTag = prodTagN('Symbol', 1)

symTable = {}
nextSymId = 0

def _new_symbol_desc(n):
    global nextSymId
    assert type(n) is str, n
    sd = (n, nextSymId)
    nextSymId += 1
    return sd
def symbol_(n): return node(symTag, _new_symbol_desc(n))
def isSymbol(x): return node_tag(x) is symTag
def symbol_desc(s):
    assert isSymbol(s)
    return s[1]
def symbol_name(s): return symbol_desc(s)[0]
def symbol_id(s): return symbol_desc(s)[1]
def symbol_eq(s1, s2): return symbol_desc(s1) is symbol_desc(s2)

def symbol(n, table=symTable):
    s = table.get(n)
    if s is None:
        s = symbol_(n)
        table[n] = s
    return s

def nameGen(alphabet=[chr(o) for o in xrange(ord('a'), ord('z')+1)]):
    rep = 0
    while True:
        repStr = str(rep)
        for s in alphabet: yield s+repStr
        rep += 1

def gensym(names=nameGen()): return symbol_(names.next()) # isolated namespace

def alias_(sym): return symbol_(symbol_name(sym))

################################################################
# envs
envTyTag, envTag = prodTagN('Env', 1)
def env_new(e): return node(envTag, e)
def env_data(e):
    assert node_tag(e) is envTag, e
    return e[1]

syntaxTyTag, syntaxTag = prodTagN('Syntax', 3)
def isSyntax(s): return node_tag(s) is syntaxTag
def syntax_new(senv, frees, form): return node(syntaxTag, senv, frees, form)
def syntax_get_(s, i): 
    assert isSyntax(s), s
    return s[i]
def syntax_senv(s): return syntax_get_(s, 1)
def syntax_frees(s): return syntax_get_(s, 2)
def syntax_form(s): return syntax_get_(s, 3)

def newSyntaxBinder(n): return (EnvName(n), (alias_(n), False))

def extendSyntaxEnv(senv, binders):
    new = Env(senv)
    for b in binders: new.extend(*b)
    return new

def applySyntaxEnv(senv, sc):
    new = env_data(syntax_senv(sc))
    frees = fromList(syntax_frees(sc))
    if frees:
        new = Env(new)
        for n in frees:
            n = EnvName(n)
            v = senv.get(n)
            if v is not None: new.extend(n, v)
    return new

def syntaxExpand(xs, senv):
    while isSyntax(xs):
        senv = applySyntaxEnv(senv, xs)
        xs = syntax_form(xs)
    return xs, senv

def isIdent(s): return isSymbol(s) or (isSyntax(s) and isIdent(syntax_form(s)))

def getIdent(senv, s):
    s, senv = syntaxExpand(s, env_data(senv))
    if isSymbol(s):
        v = senv.get(EnvName(s))
        if v is None: return None
        return v[0]
    else: assert False, s

def identEq(senv1, s1, senv2, s2):
    i1 = getIdent(senv1, s1)
    i2 = getIdent(senv2, s2)
    if i1 is i2:
        if i1 is None: return symbol_eq(s1, s2)
        return True
    return False

def uniqueBinders(bs): return len(bs) == len(set(b for b,n in bs))

def fieldsMap(*fields): return dict((EnvName(symbol(fn)), i+1)
                                    for i,fn in enumerate(fields)
                                    if fn is not None)

################################################################
# lists
listTyTag = makeType('List', (), [('ListNil', ()),
                                  ('ListCons', (None,)*2,
                                   fieldsMap('head', 'tail'))])
listNilTag, listConsTag = listTyTag.consTags
nil = node(listNilTag)
def cons(h, t): return node(listConsTag, h, t)
def cons_head(x):
    assert node_tag(x) is listConsTag, x
    return x[1]
def cons_tail(x):
    assert node_tag(x) is listConsTag, x
    return x[2]
def isListCons(x): return node_tag(x) is listConsTag
def isList(x): return x is nil or isListCons(x)
def toList(args, tail=None):
    if tail is None: tail = nil
    for x in reversed(args): tail = cons(x, tail)
    return tail
def fromList(xs):
    assert isList(xs), xs
    while xs is not nil:
        yield cons_head(xs)
        xs = cons_tail(xs)
def isList_safe(x):
    return (isinstance(x, list) and (len(x) > 0) and isinstance(x[0], NodeTag)
            and isList(x))

################################################################
# booleans
boolTyTag = makeType('Bool', (), [('False', ()), ('True', ())])
boolFalseTag, boolTrueTag = boolTyTag.consTags
true = node(boolTrueTag)
false = node(boolFalseTag)

################################################################
# literals
intTyTag, intTag = prodTagN('Int', 1)
floatTyTag, floatTag = prodTagN('Float', 1)
charTyTag, charTag = prodTagN('Char', 1)
stringTyTag, stringTag = prodTagN('String', 1)
def litInt(v): return node(intTag, v)
def litFloat(v): return node(floatTag, v)
def litChar(v): return node(charTag, v)
def litString(v): return node(stringTag, v)
def isLit(v): return node_tag(v) in (intTag, floatTag, charTag, stringTag)
def fromLit(l):
    assert isLit(l), l
    return l[1]
def unboxLit(b):
    if isLit(b): return fromLit(b)
    elif isSymbol(b): return symbol_desc(b)
    assert False, b

# todo: more specific predicates and converters can be better
def isInt(v): return node_tag(v) is intTag
def fromInt(v):
    assert isInt(v), v
    return v[1]
def isString(v): return node_tag(v) is stringTag
def fromString(v):
    assert isString(v), v
    return v[1]
################################################################
# pretty printing
def prettyList(xs): return '[%s]' % ' '.join(pretty(x) for x in fromList(xs))
def prettySymbol(s): return symbol_name(s)

def prettyInt(i): return repr(i[1])
def prettyFloat(f): return repr(f[1])
def prettyChar(c): return "'%s'"%''.join(maybeEscaped(c, "'") for c in c[1])
def prettyString(s): return '"%s"'%''.join(maybeEscaped(c, '"') for c in s[1])
def prettySyntax(s): return ('(Syntax <env> %s %s)'%
                             (#syntax_senv(s),
                              prettyList(syntax_frees(s)),
                              pretty(syntax_form(s))))

escapes = {
    '\n': '\\n',
    '\r': '\\r',
    '\t': '\\t',
    '\v': '\\v',
    }
def maybeEscaped(c, delim):
    cc = escapes.get(c)
    if cc is not None: return cc
    elif c == delim: return '\\'+delim
    return c

pretties = {undefTag: lambda _: 'Undefined', unitTag: lambda _: '()',
            symTag: prettySymbol,
            listNilTag: prettyList, listConsTag: prettyList,
            boolTrueTag: (lambda _:'True'), boolFalseTag: (lambda _:'False'),
            intTag: prettyInt, floatTag: prettyFloat, charTag: prettyChar,
            stringTag: prettyString,
            syntaxTag: prettySyntax,
            }

def pretty(v):
    if isinstance(v, list) and isinstance(v[0], NodeTag):
        pp = pretties.get(node_tag(v))
    else: pp = None
    if pp is None: return '<ugly %s>'%repr(v)
    return pp(v)

################################################################
# streams
class Stream(object):
    def __init__(self, itr):
        self.itr = itr
        self.buffer = []
    def __iter__(self): return self
    def put(self, x): self.buffer.append(x)
    def next(self):
        if self.buffer: return self.buffer.pop()
        return self.itr.next()
    def peek(self):
        x = self.next()
        self.put(x)
        return x
    def empty(self):
        if self.buffer: return False
        try: self.put(self.itr.next())
        except StopIteration: return True
        return False
    def compose(self, mkItr):
        return makeStream(mkItr(self.itr))

def makeStream(s):
    if not isinstance(s, Stream): s = Stream(s)
    return s

emptyStream = Stream(iter(()))
