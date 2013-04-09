python_eval = eval
python_apply = apply

def isSequence(expr): # should this be used?
    return isPair(expr) or isArray(expr)

def isPair(expr):
    return isinstance(expr, tuple)

def isArray(expr):
    return isinstance(expr, Array)

def head(expr):
    if isPair(expr):
        return expr[0]
    elif isArray(expr):
        return expr.head()
    assert False

def tail(expr):
    if isPair(expr):
        return expr[1]
    elif isArray(expr):
        return expr.tail()
    assert False

def isNil(expr):
    return expr is nil

def packList(xs):
    l = nil
    for x in reversed(xs):
        l = pair(x, l)
    return l

def packArray(xs):
    return Array(tuple(xs), 0)

def unpackSequence(l):
    while l is not nil:
        yield head(l)
        l = tail(l)

def isAtom(expr):
    return not isSequence(expr)

def isSymbol(expr):
    return isinstance(expr, Symbol)

def apply(env, proc, args):
    proc = execute(env, proc)
    return proc(env, args)

def analyze(expr):
    if isSequence(expr):
        return analyzeSequence(expr)
    elif isSymbol(expr):
        return lambda env: nil, env[expr]
    return lambda env: nil, expr

def analyzeSequence(expr):
    proc = analyze(head(expr))
    args = tuple(analyzeArg(x) for x in unpackSequence(tail(expr)))
    return lambda env: apply(env, proc, args)

def analyzeArg(arg):
    # pair the syntax with the analysis
    # because the proc could be a function or macro
    # so we don't know which of the two will be used beforehand
    return pair(arg, analyze(arg))

def strictArg(env, arg):
    return execute(env, tail(arg))

def lazyArg(env, arg):
    return Thunk(env, tail(arg))

def syntaxArg(env, arg):
    return head(arg)

# since this implies strictness, the result should never be a thunk
def execute(env, code):
    while env is not nil: # this looping facilitates tail calls
        env, code = code(env)
    # i think this assertion is bad... thunks can come out of the call
    # and therefore need to be evaluated?
    assert not isinstance(code, Thunk), "whoops... how did a thunk get out?"
    return code

def eval(env, expr):
    return execute(env, analyze(expr))
