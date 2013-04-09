from module import ModuleManager, Module
from expr import *
from syntax import ParseError, Parser
from StringIO import StringIO
from itertools import izip

def exprExpand(expr, attr, senv, mod, interp, tracer, isTop=False, isLHS=False):
#    print 'expr:', expr
    while not isinstance(expr, Expr):
        expr, attr, senv = macroExpand(expr, attr, senv, tracer)
        tag = node_tag(expr)
        exprExpander = exprExpanders.get(tag)
        if exprExpander is None:
            assert tag in (intTag, floatTag, charTag, stringTag), tag
            exprExpander = exprExpandLit
        result = exprExpander(expr, attr, senv, mod, interp, tracer, isTop, isLHS)
#         print 'expr:', expr
#         print 'result:', result
        expr, attr, senv = result
    return expr
def exprExpandLit(x, attr, senv, *_):
    return Construct(node_tag(x), [Lit(fromLit(x), attr)], attr), attr, senv
def exprExpandSymbol(s, attr, senv, mod, *_):
    alias = senv.get(EnvName(s))
    if alias is None: alias = mod.defVar(s, None), False
    if alias[1]: return Lit(alias[0], attr), attr, senv # todo: is this really a good idea?
    return Var(EnvName(alias[0]), attr), attr, senv
def exprExpandList(xs, attr, senv, mod, interp, tracer, isTop, isLHS):
    head, headSEnv = syntaxExpand(cons_head(xs), senv)
    if isSymbol(head):
        spec = headSEnv.get(EnvName(head))
        if spec is not None and spec[1] and isSpecial(spec[0]):
            return special_apply(spec[0], cons_tail(xs), senv, mod, interp, tracer, isTop,
                                 isLHS, attr)
    aa = cons_head(attr)
    expr = exprExpand(cons_head(xs), aa, senv, mod, interp, tracer)
    aa = cons(cons_head(attr), nil) # expanding app attr over args
    for x, a in matchedAttrs(cons_tail(xs), cons_tail(attr)):
        aa = extendPAppAttr(aa, a)
        expr = Apply(expr, exprExpand(x, a, senv, mod, interp, tracer), aa)
    return expr, attr, senv
exprExpanders = {
    symTag: exprExpandSymbol,
    listNilTag: (lambda xs, attr, *_: (Lit(unit, attr), None, None)),
    listConsTag: exprExpandList,
    }

def extendPAppAttr(pappAttr, argAttr):
    at = list(fromList(pappAttr))
    at.append(argAttr)
    return toList(at)

# macros should only return code as data, not instances of Expr
# special forms produce instances of Expr, so they must not be expanded here
def macroExpand(xs, attr, senv, tracer):
    while True:
        xs, senv = syntaxExpand(xs, senv)
        if isListCons(xs):
            head, headAttr, headSEnv = macroExpand(cons_head(xs),
                                                   cons_head(attr), senv,
                                                   tracer)
            if isSymbol(head):
                m = headSEnv.get(EnvName(head))
                if m is not None and m[1]:
                    m = m[0]
                    if isSpecial(m): break
                    elif isMacro(m):
                        xs, attr = macro_apply(m, cons(head, cons_tail(xs)),
                                               attr, senv, tracer)
                        continue
            def wrapSyn_(xs, attr, senv_):
                return wrapSyntax(senv, xs, senv_), attr
            rest = [wrapSyn_(*macroExpand(x, a, senv, tracer))
                    for x, a in matchedAttrs(cons_tail(xs),
                                             cons_tail(attr))]
            if rest: xs1, attr1 = zip(*rest)
            else: xs1, attr1 = [], []
            xs = cons(wrapSyntax(senv, head, headSEnv), toList(xs1))
            attr = cons(headAttr, toList(attr1))
        break
    return xs, attr, senv

def wrapSyntax(parentSEnv, x, senv):
    if parentSEnv is not senv: x = syntax_new(env_new(senv), nil, x)
    return x

def stripOuterSyntax(x):
    if isSyntax(x): x = syntax_form(x)
    return x

def stripSyntax(xs):
    xs = stripOuterSyntax(xs)
    if isList(xs): xs = toList([stripSyntax(x) for x in fromList(xs)])
    return xs

def evalData(code, attr, senv, mod, interp, tracer, isTop=False, isLHS=False):
    expr = exprExpand(code, attr, senv, mod, interp, tracer, isTop, isLHS)
#    return evalExpr(expr, mod.env.env, tracer)
    return evalExpr(expr, mod.env, tracer)

def getCmd(cmds, defCmd):
    cmd = None
    while cmd not in cmds:
        cmd = raw_input('%s[%s]> '%(cmds, defCmd))
        if not cmd: cmd = defCmd
    return cmd

def highlight(txt, ln, col, end):
    highlight = (' '*col)+('^'*(end-col))
    lead = '%s,%s-%s: ' % (ln, col, end)
    return lead+txt.rstrip()+'\n'+(' '*len(lead))+highlight

def fromListFlatten(xs):
    if isList_safe(xs):
        ys = []
        for x in fromList(xs): ys.extend(fromListFlatten(x))
        return ys
    return [xs]

def prettyAttr(attr):
    if attr is None: return '<unknown source>'
    elif isList_safe(attr):
        attrs = fromListFlatten(attr)
        if attrs:
            lines = {}
            for attr in attrs:
                txt, ln, col, cs = attr
                line = lines.get(ln)
                if line is None: lines[ln] = [txt, col, col+cs]
                else:
                    line[1] = min(col, line[1])
                    line[2] = max(col+cs, line[2])
            return '\n'.join(highlight(txt, ln, col, end)
                             for ln, (txt, col, end) in sorted(lines.items()))
        else: return prettyAttr(None)
    else:
        txt, ln, col, cs = attr
        return highlight(txt, ln, col, col+cs)

class Frame(object):
    def walk(self, prompt=False): yield self.show()
    def prompt(self): return getCmd(list('no'), 'n')

class EvalFrame(Frame):
    def __init__(self, expr, env): self.expr, self.env = expr, env
    def show(self):
        return '%s %s %s' % (prettyAttr(self.expr.attr), '==>', self.expr)

class ValFrame(Frame):
    def __init__(self, val): self.val = val
    def show(self): return pretty(self.val)

class SubEvalFrame(Frame):
    def __init__(self, tracer): self.tracer = tracer
    def show(self): return self.tracer.history[0].show() + ' ==> <sub-eval>'
    def walk(self, prompt=False):
        for frame in self.tracer.history:
            yield frame.show()
            cmd = 'i'
            if prompt: cmd = frame.prompt()
            if cmd == 'o': break
            elif cmd == 'i':
                for sf in frame.walk(prompt): yield sf
    def prompt(self): return getCmd(list('nio'), 'i')

class DebugTracer(Tracer):
    def __init__(self, interp, parent=None):
        self.interp = interp
        self.parent = parent
        self.history = []
    def backtrace(self):
        if self.interp.curBackTrace is not None: return
        cur = self
        bt = []
        while cur is not None:
            bt.append(cur)
            cur = cur.parent
        self.interp.curBackTrace = bt # todo: consider concurrency
    def push(self):
        subt = DebugTracer(self.interp, self)
        self.history.append(SubEvalFrame(subt))
        return subt
    def pop(self, subTracer): pass
    def add(self, expr, env):
        if env is None: frame = ValFrame(expr)
        else: frame = EvalFrame(expr, env)
        self.history.append(frame)
    def walk(self, cmd='i'):
        for s in SubEvalFrame(self).walk(True): print s

class Interpreter(object):
    promptNames = nameGen(['eval'])
    def __init__(self):
        self.modMan = ModuleManager()
        self.curBackTrace = None
        self.tagEnv = Env()
        self.rootEnv = Env()
        self.primMod = Module(EnvName(symbol_('__primitives')), self.rootEnv)
        loadPrims(self.tagEnv, self.primMod)
    def streamValues(self, mod, stream, tracer):
        exprs = makeStream(Parser(mod.ops).parse(stream))
        while True:
            try: expr, attr = exprs.next()
            except ParseError, e: raise
            try: yield evalData(expr, attr, mod.senv, mod, self,
                                tracer, True, True)
            except Thrown, e:
                print 'unhandled error:', attr
                raise
            except TypeError, e:
                print 'type error:', attr
                raise
            except Exception, e:
                print 'error at:', attr
                raise
    def evalModule(self, mod, stream, tracer, parallel=(lambda:None)):
        result = unit
        for result in self.streamValues(mod, stream, tracer): parallel()
        return result
    def includePrims(self, mod):
        syms = self.primMod.modeSyms()
        mod.include(self.primMod, izip(syms, syms), None, mod.private)
    def importModule(self, fileName, tracer, finish=True):
        def onNew(mod):
            self.includePrims(mod)
            self.evalModule(mod, open(fileName), tracer)
            return finish
        return self.modMan.get(EnvName(symbol(fileName)), self.rootEnv, onNew)
    def loadRepl(self):
        def onNew(mod):
            self.includePrims(mod)
            return False
        return self.modMan.get(EnvName(gensym(self.promptNames)), self.rootEnv,
                               onNew)
    def interact(self, mod, tracer):
        import readline # magically enhances raw_input
        buffer = []
        prompt1 = '%s> ' % str(mod.name)
        prompt2 = ('.'*(len(prompt1)-1)) + ' '
        line = raw_input(prompt1)
        while line:
            buffer.append(line+'\n')
            line = raw_input(prompt2)
        return self.evalModule(mod, StringIO(''.join(buffer)), tracer)

def repl(interpreter, tracer, mod=None):
    if mod is None: mod = interpreter.loadRepl()
    try:
        while True:
            result = interpreter.interact(mod, tracer)
            if result is not unit: print pretty(result)
    except EOFError: pass
    print

def matchedAttrs(args, attrs):
    args = list(fromList(args))
    attrs = list(fromList(attrs))
    assert len(attrs) == len(args), (len(args), len(attrs),
                                     pretty(toList(args)),
                                     pretty(toList(attrs)))
    return zip(args, attrs)

def validateArgs(args, n, attr):
    matched = matchedAttrs(args, cons_tail(attr))
    if len(matched) != n:
        evalError('invalid number of arguments; expected %d, received %d'
                  %(n, len(matched)), attr)
    return matched

def nameFromSym(n, default=None):
    if n is nil: return default
    else: return symbol_name(n)

def makeRecord(args, attr, senv, mod, interp, tracer, isTop, isLHS):
    (name,_), (body, bodyAttr) = validateArgs(args, 2, attr)
    def expandBinding((binding, bindingAttr)):
        (bname,_), (bval,bvalAttr) = matchedAttrs(binding, bindingAttr)
        return bname, exprExpand(bval, bvalAttr, senv, mod, interp, tracer)
    cr = ConsRecord([expandBinding(bind)
                     for bind in matchedAttrs(body, bodyAttr)],
                    nameFromSym(name, ConsRecord.names.next()), attr)
    return cr, attr, senv

def defSyntax(args, attr, senv, mod, interp, tracer, isTop, isLHS):
    # todo: isTop checked by senv=? # actually check isLHS
    if not isTop:
        evalError('syntax may only be defined at the top level', attr)
    (sym, _), (body, bodyAttr) = validateArgs(args, 2, attr)
    if not isSymbol(sym):
        evalError("invalid name '%s'"%sym, attr)
    body = evalData(body, bodyAttr, senv, mod, interp, tracer)
    assert isSpecial(body) or isMacro(body), body
    mod.defSyntax(sym, body)
    return nil, attr, senv

def defVar(args, attr, senv, mod, interp, tracer, isTop, isLHS):
    if not isLHS:
        evalError('variables may only be defined at the module/record level',
                  attr)
    (sym, _), (body, bodyAttr) = validateArgs(args, 2, attr)
    if not isSymbol(sym): evalError("invalid name '%s'"%sym, attr)
    rhs = evalData(body, bodyAttr, senv, mod, interp, tracer)
    assert not isSpecial(rhs) or isMacro(rhs), rhs
    mod.defVar(sym, rhs)
    return nil, attr, senv

# todo: allows a type ambiguity between tag envs and senvs; should separate
# that is, tag envs could be incorrectly passed as senv arguments
def exposeTags(args, attr, senv, mod, interp, *_):
    validateArgs(args, 0, attr)
    return Lit(env_new(interp.tagEnv), attr), attr, senv

# redefined as a macro
#def quote(args, attr, senv, *_):
#    (body, _) = validateArgs(args, 1, attr)[0]
#    return Lit(stripSyntax(body), attr), attr, senv

def makeUnboxed(args, attr, senv, *_):
    (val, _) = validateArgs(args, 1, attr)[0]
    return Lit(unboxLit(stripSyntax(val)), attr), attr, senv

def makeAbstraction(args, attr, senv, mod, interp, tracer, *_):
    (name,_), (var, varAttr), (body, bodyAttr) = validateArgs(args, 3, attr)
    if not isSymbol(var): evalError('bound variable is not a symbol', varAttr)
    senv = extendSyntaxEnv(senv, [newSyntaxBinder(var)])
    body = exprExpand(body, bodyAttr, senv, mod, interp, tracer)
    return Abstract(EnvName(senv.get(EnvName(var))[0]), body,
                    nameFromSym(name), attr), attr, senv

def makeMacro(args, attr, senv, mod, interp, tracer, *_):
    proc, procAttr = validateArgs(args, 1, attr)[0]
    proc = evalData(proc, procAttr, senv, mod, interp, tracer)
    if not isProc(proc): evalError('macro value must be a procedure', attr)
    return Lit(macro_new(proc, senv), attr), attr, senv

opAssoc = dict(left=False, right=True)
def defOperator(args, attr, senv, mod, *_): # todo: push and pop opEnv (letOp)
    (name,_), (fixity,_), (assoc,_), (prec,_) = validateArgs(args, 4, attr)
    rightAssoc = opAssoc[symbol_name(assoc)]
    assert isSymbol(name), name
    mod.defOp(name, symbol_name(fixity), rightAssoc, fromLit(prec))
    return nil, attr, senv

unitPattern = LitPattern(unit)
defPattern = DefPattern()

def makeQuotePattern(expr, tagEnv, senv, interp):
    if isSymbol(expr): return LitPattern(expr)
    elif isList(expr):
        xs = CompoundPattern([LitPattern(listNilTag)])
        for x in reversed(tuple(fromList(expr))):
            xs = CompoundPattern([LitPattern(listConsTag),
                                  makeQuotePattern(x, tagEnv, senv, interp), xs])
        return xs
    else: return makePattern(expr, tagEnv, senv, interp)[0]

def isPSym(psym, interp, senv, sym):
    return identEq(env_new(senv), sym, env_new(interp.primMod.senv), psym)

def isQuote(*args): return isPSym(symbol("quote"), *args)
def isUnbox(*args): return isPSym(symbol("__unboxed"), *args)

def makePattern(expr, tagEnv, senv, interp, listHead=False):
    binders = []
    if isIdent(expr):
        tag = symToTag(expr, senv, interp, listHead)
        if expr is symbol('_'): pat = defPattern
        else:
            if tag is None:
                binder = newSyntaxBinder(expr)
                binders.append(binder)
                pat = VarPattern(binder[1][0])
            else:
                tag = LitPattern(tag)
                if listHead: pat = tag
                else: pat = CompoundPattern([tag])
    elif expr is nil: pat = unitPattern
    elif isLit(expr): pat = CompoundPattern([LitPattern(node_tag(expr)),
                                             LitPattern(fromLit(expr))])
    elif isList(expr):
        expr = tuple(fromList(expr))
        potential = len(expr) > 0 and isSymbol(expr[0])
        if potential and isQuote(interp, senv, expr[0]):
            assert len(expr) == 2, expr
            pat = makeQuotePattern(expr[1], tagEnv, senv, interp)
        elif potential and isUnbox(interp, senv, expr[0]):
            assert len(expr) == 2, expr
            pat = LitPattern(unboxLit(expr[1]))
        else:
            tag, binds = makePattern(expr[0], tagEnv, senv, interp, True)
            binders.extend(binds)
            args = []
            for se in expr[1:]:
                subp, binds = makePattern(se, tagEnv, senv, interp)
                args.append(subp)
                binders.extend(binds)
            pat = CompoundPattern([tag]+args)
    else: assert False, expr
    return pat, binders

def symToTag_(sym, senv, tagEnv, check=False):
    sym, senv = syntaxExpand(sym, senv)
    sym = senv.get(EnvName(sym))
    if sym is None or sym[1]: tag = None
    else: tag = tagEnv.get(EnvName(sym[0]))
    if check:
        assert tag is not None, (("invalid constructor tag '%s'"%EnvName(sym))
                                 +str(symbol_id(sym)))
    return tag

def symToTag(sym, senv, interp, check=True):
    return symToTag_(sym, senv, interp.tagEnv, check)

def makeSwitchPattern(expr, senv, interp, attr, isTagNotVal=None):
    expr, senv = syntaxExpand(expr, senv)
    def check(b):
        if isTagNotVal is not None and isTagNotVal!=b:
            evalError('cannot mix tag and value alternatives', attr)
    if isSymbol(expr): # constructor tag
        tag = symToTag(expr, senv, interp)
        check(True)
        return tag, True
    elif isList(expr): # unboxed val
        expr = tuple(fromList(expr))
        ub, ubsenv = syntaxExpand(expr[0], senv)
        if isUnbox(interp, ubsenv, ub):
            assert len(expr) == 2, expr
            check(False)
            return unboxLit(syntaxExpand(expr[1], senv)[0]), False
        else: assert False, ("expected an unboxed value pattern", expr)
    else: assert False, ("unknown pattern", expr)

def makeSwitch(args, attr, senv, mod, interp, tracer, *_):
    matched = matchedAttrs(args, cons_tail(attr))
    def expand(ex, at):
        return exprExpand(ex, at, senv, mod, interp, tracer)
    discrim = expand(*matched[0])
    default = expand(*matched[1])
    alts = {}
    pty = None
    for altex in matched[2:]:
        (pats, pAttr), (body, bodyAttr) = matchedAttrs(*altex)
        body = expand(body, bodyAttr)
        pats, psenv = syntaxExpand(pats, senv)
        for pat in fromList(pats):
            pat, pty = makeSwitchPattern(pat, psenv, interp, attr, pty)
            assert pat not in alts, pat
            alts[pat] = body
    return Switch(discrim, default, alts, attr), attr, senv

def makeUnpack(args, attr, senv, mod, interp, tracer, *_):
    (tag, _), (index, _), (node, nodeAttr) = validateArgs(args, 3, attr)
    tag = symToTag(tag, senv, interp)
    index = fromInt(index)
    node = exprExpand(node, nodeAttr, senv, mod, interp, tracer)
    return Unpack(index, tag, node, attr), attr, senv

def expandPattern(p, pAttr, senv, mod, interp, tracer):
    p, _, _ = macroExpand(p, pAttr, senv, tracer)
    return makePattern(stripSyntax(p), interp.tagEnv, senv, interp)

def makeCase(args, attr, senv, mod, interp, tracer, *_):
    matched = matchedAttrs(args, cons_tail(attr))
    arg = exprExpand(matched[0][0], matched[0][1], senv, mod, interp, tracer)
    alts = []
    for alt, at in matched[1:]:
        (p, pAttr), (body, bodyAttr) = matchedAttrs(alt, at)
        pat, binders = expandPattern(p, pAttr, senv, mod, interp, tracer)
        assert uniqueBinders(binders), [b for b,v in binders]
        body = exprExpand(body, bodyAttr, extendSyntaxEnv(senv, binders), mod,
                          interp, tracer)
        alts.append((pat, body))
    return Case(arg, alts, attr), attr, senv

# def makeTypeTagged(args, attr, senv, mod, interp, tracer, *_):
#     (expr, exprAt), = validateArgs(args, 1, attr)
#     return TypeTagged(exprExpand(expr, exprAt, senv, mod, interp, tracer),
#                       attr), attr, senv

def makeLetRec(args, attr, senv, mod, interp, tracer, *_):
    bindings, (body, bodyAttr) = validateArgs(args, 2, attr)
    bs = []
    names = []
    vals = []
    for binding in matchedAttrs(*bindings):
        (var, _), (val, valAt) = matchedAttrs(*binding)
        binder = newSyntaxBinder(var)
        bs.append(binder)
        names.append(EnvName(binder[1][0]))
        vals.append((val, valAt))
    assert uniqueBinders(bs), [b for b,v in bs]
    bodySEnv = extendSyntaxEnv(senv, bs)
    return LetRec(names, [exprExpand(v, vat, bodySEnv, mod, interp, tracer)
                          for v, vat in vals],
                  exprExpand(body, bodyAttr, bodySEnv, mod, interp, tracer),
                  attr), attr, senv

def makeData(args, attr, senv, mod, interp, tracer, isTop, *_):
    (tyName,_), (tyParams,_), (consTags,consAts) = validateArgs(args, 3, attr)
    tyParams = tuple(fromList(tyParams)) # todo: build type exprs
    ctags = []
    cats = []
    for ctag, cat in matchedAttrs(consTags, consAts):
        cats.append(cat)
        (cName,_), (fields,_) = matchedAttrs(ctag, cat)
        fnames = {}
        argTypes = []
        for fi, field in enumerate(fromList(fields)):
            if isList(field) and cons_head(field) is symbol('::'):
                xs = fromList(cons_tail(field))
                fname, ftype = xs
                assert isSymbol(fname)
                fnames[EnvName(fname)] = fi+1
                argTypes.append(ftype)
            else: argTypes.append(field)
        ctags.append((symbol_name(cName), argTypes, fnames))
    tyTag = makeType(symbol_name(tyName), tyParams, ctags)
    for ctag, cat in izip(tyTag.consTags, cats):
        nargs = len(ctag.argTypes)
        if nargs == 0: cns = node(ctag) # singletons can be shared
        else: cns = evalExpr(makeConstructor(ctag, nargs, ctag.n, cat),
                             mod.env, tracer)
        alias = mod.defVar(symbol(ctag.n), cns)
        interp.tagEnv.extend(EnvName(alias), ctag)
    # todo: type properties/functions record
    alias = mod.defVar(tyName, nil) # todo: an actual typetag record
    interp.tagEnv.extend(EnvName(alias), tyTag)
    return nil, attr, senv

def makeConstructor(tag, num, name, attr):
    argNames = [symbol(n) for n, i in izip(nameGen(['C_arg']), xrange(num))]
    cns = Construct(tag, [Var(EnvName(argn), attr) for argn in argNames], attr)
    for argn, i in izip(reversed(argNames), xrange(num)):
        cns = Abstract(EnvName(argn), cns, '__C%d_%s'%(i+1, name), attr)
    return cns

def makeCall(args, attr, senv, mod, interp, tracer, *_):
    (prim, primAttr), (params, paramsAttr) = validateArgs(args, 2, attr)
    def _ex(v, a): return exprExpand(v, a, senv, mod, interp, tracer)
    return (Call(_ex(prim, primAttr),
                [_ex(p, a) for p,a in matchedAttrs(params, paramsAttr)], attr),
            attr, senv)

def makeCatch(args, attr, senv, mod, interp, tracer, *_):
    (body, bodyAttr), (catch, catchAttr) = validateArgs(args, 2, attr)
    return Catch(exprExpand(body, bodyAttr, senv, mod, interp, tracer),
                 exprExpand(catch, catchAttr, senv, mod, interp, tracer),
                 catchAttr, attr), attr, senv

def makeImport(args, attr, senv, mod, interp, tracer, isTop, isLHS):
    (modName, mnAttr), (fileName, fnAttr), = validateArgs(args, 2, attr)
    assert node_tag(fileName) is stringTag
    fileName = fromLit(fileName)
    nm = interp.importModule(fileName, tracer)
    nm = node(ModuleTag(nameFromSym(modName, fileName)), nm)
    return Lit(nm, attr), attr, senv

def makeInclude(args, attr, senv, mod, interp, tracer, isTop, isLHS):
    (modVal, modAttr), (namePairs, fAttr), = validateArgs(args, 2, attr)
    mv = evalData(modVal, modAttr, senv, mod, interp, tracer)
    nps = evalData(namePairs, fAttr, senv, mod, interp, tracer)
    mod.include(module_data(mv), [fromList(np) for np in fromList(nps)])
    return nil, attr, senv

def makeGetMode(args, attr, senv, mod, interp, tracer, isTop, isLHS):
    validateArgs(args, 0, attr)
    return Lit(mod.getMode().sym, attr), attr, senv

def makeSetMode(args, attr, senv, mod, interp, tracer, isTop, isLHS):
    (mode, modeAttr), = validateArgs(args, 1, attr)
    mod.setMode(EnvName(evalData(mode, modeAttr, senv, mod, interp, tracer)))
    return nil, attr, senv

def makeExprN_(cls, n):
    def make_(args, attr, senv, mod, interp, tracer, *_):
        args = validateArgs(args, n, attr)
        def ex((e, ea)): return exprExpand(e, ea, senv, mod, interp, tracer)
        return cls(*([ex(arg) for arg in args]+[attr])), attr, senv
    return make_

def makeExpr1_(cls): return makeExprN_(cls, 1)
def makeExpr2_(cls): return makeExprN_(cls, 2)
# def makeExpr3_(cls): return makeExprN_(cls, 3)
# def makeExpr1_(cls):
#     def make_(args, attr, senv, mod, interp, tracer, *_):
#         (arg, argAttr), = validateArgs(args, 1, attr)
#         return cls(exprExpand(arg, argAttr, senv, mod, interp, tracer)), attr, senv
#     return make_

makeDelay = makeExpr1_(delay)
makeForce = makeExpr1_(Force)

# def makeGetField(args, attr, senv, mod, interp, tracer, *_):
#     fname, lhs = validateArgs(args, 2, attr)
#     def ex((e, ea)): return exprExpand(e, ea, senv, mod, interp, tracer)
#     return GetField(ex(fname), ex(lhs), attr), attr, senv

makeGetField = makeExpr2_(GetField)
# makeSetField = makeExpr2_(SetField)

def prettyPrint(v):
    print pretty(v)
    return unit, None

def macroExpand_(senv, form):
    form, _, se = macroExpand(form, expandAttr(form, {}, None), env_data(senv),
                              Tracer()) # no way to thread outer tracer yet
    return wrapSyntax(env_data(senv), form, se), None

def throw(exc): raise Thrown(exc)

def destructuredForm(tagEnv, senv, sym): # preferable to constructorSet?
    ctag = symToTag_(sym, env_data(senv), env_data(tagEnv))
    if ctag is None: return nil
    return toList([ctag]+[litInt(i) for i in range(len(ctag.argTypes))])

def constructorSet(tagEnv, senv, sym):
    tag = symToTag_(sym, env_data(senv), env_data(tagEnv))
    if tag is None: return nil
    names = nameGen()
    def conForm(ctag):
        return toList([symbol(ctag.n), toList([symbol_(names.next())
                                               for at in ctag.argTypes])])
    return cons(conForm(tag),
                toList([conForm(ctag)
                        for ctag in tag.tyTag.consTags if ctag is not tag]))

# todo: letSyntax, letOp?
# todo: arith, cmp

# throw (unwind in llvm), throwTo (throw to another thread; llvm how? signals?)
# try/catch (invoke in llvm)

def simpleCall(f):
    return (lambda *args: (f(*args), None))
def rebool(f):
    def rebooled(*args):
        if f(*args): return true
        else: return false
    return rebooled

def gensymPrim(): return gensym()

coreSpecials = dict(
#    quote=quote,
    __operator=defOperator,
    __unpack=makeUnpack,
#    __pack=makePack,
    __tags=exposeTags,
    __record=makeRecord,
    __get_field=makeGetField,
    __def_var=defVar,
    __def_syntax=defSyntax,
    __macro=makeMacro,
    __abstract=makeAbstraction,
    __call=makeCall,
    __case=makeCase, # todo: elim
    __switch=makeSwitch,
    __letrec=makeLetRec,
    __catch=makeCatch,
    __data=makeData,
    __unboxed=makeUnboxed,
#    __type_tagged=makeTypeTagged,
    __import=makeImport, # todo: makeModule instead?
    __include=makeInclude, # not needed with new __def_syntax rebind semantics?
    __get_mode=makeGetMode,
    __set_mode=makeSetMode,
#    __paste=makePaste, # unnecessary; can implement with eval and file-reading
    delay=makeDelay,
    __force=makeForce,
    )

corePrims = dict(
    __string_to_symdesc=(lambda s: (symbol(fromString(s)), None)),
    __identical=simpleCall(rebool(lambda a,b: a is b)), ## == for unboxed vals?
    __tag=simpleCall(node_tag),
    __fieldNames=simpleCall(lambda node: node_tag(node).fieldSyms(node)),
    __destructured_form=simpleCall(destructuredForm),
    __constructor_set=simpleCall(constructorSet),
#    __get_field=simpleCall(node_field),
    __print=prettyPrint,
    __syntactic_closure=simpleCall(syntax_new),
    __macro_expand=macroExpand_,
#    __is_ident=simpleCall(rebool(isIdent)),
    __ident_eq=simpleCall(rebool(identEq)),
#    __is_cons=,
    __throw=throw,
    __gensym=simpleCall(gensymPrim),
    )

def make_constr(tag):
    return evalExpr(makeConstructor(tag, tag.numFields(), tag.n, None),
                    Env(), Tracer())
coreConstrs = {
    'Syntax': syntaxTag,
    'Symbol': symTag,
    ':': listConsTag,
    'Char': charTag,
    'String': stringTag,
    'Float': floatTag,
    'Int': intTag,
    }
coreVals = {
    'Unit': unit,
    'Nil': nil,
    'True': true,
    'False': false,
    }
coreTags = {
    'Nil': listNilTag,
    'True': boolTrueTag,
    'False': boolFalseTag,
    }
coreTags.update(coreConstrs)
coreVals.update((name, make_constr(tag)) for name, tag in coreConstrs.iteritems())
coreTyTags = dict(
#     Env=envTyTag,
#     Syntax=syntaxTyTag,
#     Symbol=symTyTag,
#     List=listTyTag,
#     Bool=boolTyTag,
#     Int=intTyTag,
#     Float=floatTyTag,
#     Char=charTyTag,
#     String=stringTyTag,
)
coreOps = (
    (':', 'infix', True, 4),
    )

def loadPrims(tagEnv, mod):
    for n, tt in coreTyTags.iteritems():
        sym = symbol(n)
        alias = mod.defVar(sym, nil) # todo: an actual typetag record
        tagEnv.extend(EnvName(alias), tt)
    for n, p, in coreSpecials.iteritems():
        mod.defSyntax(symbol(n), special_new(prim_new(primTag(n), p)))
    for n, p, in corePrims.iteritems():
        mod.defVar(symbol(n), prim_new(primTag(n), p))
    for n, v, in coreVals.iteritems():
        alias = mod.defVar(symbol(n), v)
        tag = coreTags.get(n)
        if tag is not None: tagEnv.extend(EnvName(alias), tag)
    for op in coreOps: mod.defOp(symbol(op[0]), *op[1:])
