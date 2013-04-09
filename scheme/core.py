# TODO: assertion violations should signal conditions in the language?
class Array(object):
    def __init__(self, xs, index):
        assert (index >= 0) and (len(xs) > index)
        self.xs = xs
        self.index = index
    def head(self):
        return self.xs[self.index]
    def tail(self):
        next = self.index+1
        xs = self.xs
        if next < len(xs):
            return Array(xs, next)
        return nil
    def length(self):
        return len(self.xs) - self.index
    def __getitem__(self, index):
        assert index >= 0
        index += self.index
        assert index < len(self.xs)
        return self.xs[index]

class NamedObject(object):
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return self.name
    def __repr__(self):
        return self.name

nil = topLevel("nil")(NamedObject("nil"))
#true = NamedObject("true")
#false = NamedObject("false")
# these really are the most convenient for expedient development in python
# the lowercase spellings can be worked out with pretty-printing
true = topLevel("true")(True)
false = topLevel("false")(False)


class Symbol(object):
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return self.name
    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, str(self))


class Thunk(object):
    def __init__(self, env, code):
        self.env = env
        self.code = code
    def eval(self):
        if self.env is not None
            self.code = execute(self.env, self.code)
            self.env = None
        return self.code
    def __call__(self, env):
        return nil, self.eval()


def symbolToStr(symbol):
    assert isinstance(symbol, Symbol), repr(symbol)
    return str(symbol)


class Environment(object):
    def __init__(self, bindings, parent=None):
        self.parent = parent
        self.env = dict((symbolToStr(k), v) for k, v in bindings)
    def __getitem__(self, key):
        name = symbolToStr(key)
        if (self.env.has_key(name)):
            return self.env[name]
        assert self.parent is not None, ("The symbol '%s' is not bound"
                                         % key)
        return self.parent[key]
    def __setitem__(self, key, value):
        name = symbolToStr(key)
        if (self.env.has_key(name)):
            self.env[name] = value
        else:
            assert self.parent is not None, ("The symbol '%s' is not bound"
                                             % key)
            self.parent[key] = value
    # ideally this is only necessary for an interactive interpreter
    # it's also useful for implementing recursion... cool
    def define(self, key, value):
        self.env[symbolToStr(key)] = value
    def __repr__(self):
        result = "%s(" % self.__class__.__name__
        if self.parent is not None:
            result += repr(self.parent) + "\n---"
        for item in self.env.iteritems():
            result += "\n"+repr(item)
        return result+")"


class Applicable(object):
    def __init__(self, env, strategy, params, body):
        self.env = env
        self.strategy = strategy
        self.params = params
        self.body = body
    def __call__(self, env, args):
        return eval(env, makeList(self.strategy,
                                  self.env, self.params, args, self.body))


def syntaxStrategy(env, args):
    parentEnv, params, args, body = args
    bindings = ((param, arg) for param, arg in
                orderedParamsAndArgs(params, args))
    return Environment(bindings, parentEnv), body


def strictStrategy(env, args):
    parentEnv, params, args, body = args
    bindings = ((param, eval(env, arg)) for param, arg in
                orderedParamsAndArgs(params, args))
    return Environment(bindings, parentEnv), body


def orderedParamsAndArgs(params, args):
    while isPair(params):
        yield head(params), head(args)
        params = tail(params)
        args = tail(args)
    if not isNil(params):
        yield params, args
        args = nil
    assert isNil(args)
