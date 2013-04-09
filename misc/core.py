# Copyright (C) 2007 Gregory L. Rosenblatt
# All rights reserved

# <greg.uriel@gmail.com>
# http://code.google.com/p/uriel/

# This file is part of Uriel.

# Uriel is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# Uriel is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this library.  If not, see <http://www.gnu.org/licenses/>

""" Core system used to implement the language runtime """

from itertools import izip

topLevelDict = {}

def topLevel(key):
    def addToTop(value):
        topLevelDict[key] = value
        return value
    return addToTop


class NamedObject(object):
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return self.name

nil = topLevel("nil")(NamedObject("nil"))
#true = NamedObject("true")
#false = NamedObject("false")
# these really are the most convenient for expedient development in python
# the lowercase spellings can be worked out with pretty-printing
true = topLevel("true")(True)
false = topLevel("false")(False)


def makeTopEnvironment():
    keys, values = zip(*topLevelDict.iteritems())
    keys = (Symbol(k) for k in keys)
    return Environment(keys, values)


def isNil(x):
    return strictValue(x) is nil

def isTrue(x):
    return not isFalse(x)

def isFalse(x):
    return isEq(x, false)

def isEq(a, b):
    if (isinstance(a, Symbol) and
        isinstance(b, Symbol)):
        return str(a) == str(b)
    return a is b

def isEqual(a, b):
    return a == b # actually this should probably be defined in lisp

def isAtom(a):
    return not isArray(a)

# this will probably change to scrutinize more type info
# as arrays may be used for data structures that can be considered
# self-evaluating as well
# actually that might not be true... data structures aren't atoms
# the form (constructor, arg1, arg2, etc.) IS an applicative form
# and so they can only be returned by applications, so there is no problem
# so arrays should never be considered self-evaluating... yay
def isSelfEvaluating(a):
    return not isArray(a)

# arrays
def isArray(a):
    a = strictValue(a)
    return isinstance(a, list)

def makeArray(*args): # a generalization beyond cons
    return list(args)

def arrayIndex(array, index):
    array = strictValue(array)
    assert isArray(array)
    return array[index]

def setArrayIndex_(array, index, value):
    array[index] = value


# pairs
def isPair(a): # pair?
    return isArray(a) and len(strictValue(a)) == 2

def makePair(a, b): # cons: this could be defined in the language given arrays
    return makeArray(a, b)

def pairHead(p): # car
    assert isPair(p), repr(strictValue(p))
    return arrayIndex(p, 0)

def pairTail(p): # cdr
    assert isPair(p)
    return arrayIndex(p, 1)

def setPairHead_(p, value):
    setArrayIndex(p, 0, value)

def setPairTail_(p, value):
    setArrayIndex(p, 1, value)


def applyStrict(func, args, env):
    args = strictValues(env, *listElements(args))
    return func(*args), None

def applyLazy(func, args, env):
    args = lazyValues(env, *listElements(args))
    return func(*args), None

# not needed since you write macros using python code itself atm...
# macro meta-programming is sort of equivalent to writing directly in python
#def applyMacro(func, args, env):
#    args = macroValues(env, *listElements(args))
#    return func(*args), None

def makeApplyFunc(applyType):
    def makeFunc(func):
        def applyFunc(args, env):
            return applyType(func, args, env)
        return applyFunc
    return makeFunc

makeStrictFunc = makeApplyFunc(applyStrict)
makeLazyFunc = makeApplyFunc(applyLazy)

applyIsEq = topLevel("eq?")(makeStrictFunc(isEq))
applyIsEqual = topLevel("equal?")(makeStrictFunc(isEqual))

applyPairHead = topLevel("head")(makeStrictFunc(pairHead))
applyPairTail = topLevel("tail")(makeStrictFunc(pairTail))
applyMakePair = topLevel("pair")(makeLazyFunc(makePair))

# a way to extract the current environment
@topLevel("current-env")
def applyCurrentEnv(args, env):
    assert not tuple(listElements(args))
    return env, None

@topLevel("quote")
def applyQuote(args, env):
    value, = listElements(args)
    return value, None

# consequent and alternative are lazy, predicate is strict
@topLevel("if")
def applyIf(args, env):
    predicate, consequent, alternative = listElements(args)
    if isTrue(evalStrict(predicate, env)):
        return consequent, env
    return alternative, env

# any use for a cond->if transform?
# cond might be a better generalization instead of if
# from a jump-table/case-analysis compilation point of view
# or maybe a special case construct should be provided instead
# 
#def applyCond(*args):
#    args = iter(args)
#    try:
#        while True:
#            if isTrue(evalStrict(args.next())):
#                return args.next()
#            args.next()
#    except StopIteration:
#        pass
#    return false
    

class Thunk(object):
    def __init__(self, exp, env):
        self.exp = exp
        self.env = env
    def eval(self):
        if hasattr(self, "exp"):
            self.value = evalStrict(self.exp, self.env)
            del self.exp
            del self.env
        return self.value


def isList(a): # list?
    return isPair(a) or isNil(a)

def makeList(*args): # create a lisp-list from the arguments
    l = nil
    for a in reversed(args):
        l = makePair(a, l)
    return l

def listElements(l): # generator to extract the values in a lisp-list
    while not isNil(l):
        assert isPair(l), repr(l)
        yield pairHead(l)
        l = pairTail(l)

def strictValues(env, *args):
    for a in args:
        yield evalStrict(a, env)

def lazyValues(env, *args):
    for a in args:
        yield evalLazy(a, env)

#def macroValues(env, *args): # pass on the syntactic expressions directly
#    for a in args:
#        yield a

# separate eval from analyze?
#(define (eval exp env)
#  ((analyze exp) env))

#def eval(expr, env):
#    if isAtom(expr):
#        if type(expr) is Symbol:
#            return env[str(expr)]
#        return expr
#    return apply(pairHead(expr), pairTail(expr), env)

def isVariable(expr):
    return isinstance(expr, Symbol)

def evalAtom(expr, env):
    if isVariable(expr):
        return env[expr]
    return expr

# constant space eval (for tail calls?)
def eval(expr, env):
    while env is not None: # this looping facilitates tail calls
        if isinstance(expr, Thunk):
            expr = expr.eval()
        elif isAtom(expr):
            return evalAtom(expr, env)
        else:
            func = evalStrict(pairHead(expr), env)
            args = pairTail(expr)
            result = apply(func, args, env)
            #        print "apply result:"
            #        print func
            #        print args
            #        print result
            expr, env = result
    return expr

# the future eval?
# actually, i think pre-analysis won't help python performance
# it's something to save for an interpreter built in the language itself
# compilation to python byte codes is a better focus for performance
# the prototype core in this file is really only for testing and boostrapping
#def eval_(expr, env):
#    value = analyze(expr)
#    while env is not None:
#        value, env = value(env)
#    return value

# syntactically analyze an expr and return a function taking an env
# that will evaluate that expr with the given env
#def analyze(expr):
#    pass

def evalLazy(expr, env):
    if isAtom(expr):
        return evalAtom(expr, env)
    return Thunk(expr, env)

def evalStrict(expr, env):
    return strictValue(eval(expr, env))

def evalSyntax(expr, env):
    return expr

applyEval = topLevel("eval")(makeStrictFunc(eval))
applyEvalLazy = topLevel("lazy")(makeStrictFunc(evalLazy))
applyEvalStrict = topLevel("strict")(makeStrictFunc(evalStrict))
applyEvalSyntax = topLevel("syntax")(makeStrictFunc(evalSyntax))

def evalSequence(exprs, env):
    while True:
        expr = pairHead(exprs)
        next = pairTail(exprs)
        if isNil(next):
            break
        eval(expr, env)
        exprs = next
    return expr, env # facilitate tail calls


# force?
def strictValue(x):
    if isinstance(x, Thunk):
        return x.eval()
    return x

# consider parallelization here
# evaluation nodes could be working on arguments simultaneously
# or maybe use a special primitive that deals with parallelization details
# ie. eval-arguments
#def apply(func, args, env): 
#    if isinstance(func, function) or isinstance(func, partial):
#        return callPyFunc(func, args, env)
#    return func(args, env)

def apply(func, args, env):
    return func(args, env)

#@topLevel("apply")
def applyApply(args, env):
    func, args = strictValues(env, *listElements(args))
    args = makeList(tuple(
        makeList(Symbol("quote"), arg) for arg in listElements(args)))
#    print "apply apply:", args
    return apply(func, args, env)

# where func returns an expr representing its body and its extendeded env
# for evaluating that expr

from new import function # as a type to compare to
from functools import partial

def getArgCount(func):
    if isinstance(func, partial):
        return func.func.func_code.co_argcount - len(func.args)
    return func.func_code.co_argcount


# two forms of py functions? those that represent basic operators and return
# actual values that no longer need to be applied
# and those that represent syntax and return expressions with environments
# for continued application


class Symbol(object):
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return self.name
    def __repr__(self):
        return "%s(%r)" % (self.__class__.__name__, str(self))


class Environment(object):
    def __init__(self, keys, values, parent=None):
        keys = tuple(keys)
        values = tuple(values)
        assert len(keys) == len(values)
        self.parent = parent
        # assert that all keys are symbols (this could be a more complicated
        # "naming" type other than symbol that includs additional information
        # useful for debugging, but for now just assume they're symbols
        for k in keys:
            assert isinstance(k, Symbol), repr(k)
        self.env = dict((str(k), v) for k, v in izip(keys, values))
    def __getitem__(self, key):
        assert isinstance(key, Symbol)
        if (self.env.has_key(str(key))):
            return self.env[str(key)]
        assert self.parent is not None, ("The symbol '%s' is not bound"
                                         % key)
        return self.parent[key]
    def __setitem__(self, key, value):
        assert isinstance(key, Symbol)
        if (self.env.has_key(str(key))):
            self.env[str(key)] = value
        else:
            assert self.parent is not None, ("The symbol '%s' is not bound"
                                             % key)
            self.parent[key] = value
    # ideally this is only necessary for an interactive interpreter
    # it's also useful for implementing recursion... cool
    def define(self, key, value):
        assert isinstance(key, Symbol)
        self.env[str(key)] = value
    def __repr__(self):
        result = "%s(" % self.__class__.__name__
        if self.parent is not None:
            result += repr(self.parent) + "\n---"
        for item in self.env.iteritems():
            result += "\n"+repr(item)
        return result+")"


@topLevel("define")
def define(args, env):
    symbol, value = listElements(args)
    # force setting this symbol in the current env
    env.define(strictValue(symbol), evalStrict(value, env))
    return value, None

@topLevel("set!")
def set_(args, env):
    symbol, value = listElements(args)
    # only set this symbol if it exists already
    env[strictValue(symbol)] = evalStrict(value, env)
    return value, None

# define letrec, or at least some sort of "rec" so that letrec can be
# implemented in the lisp code without the inefficient Y-combinator
# things like "define" and "let" can be implemented in the lisp itself?
@topLevel("letrec-list")
def letRec(args, env):
    # this old way won't necessarily give an error with incorrect arguments
#    names, values = izip(*(listElements(p) for p in listElements(args)))
    names = []
    values = []
    for p in listElements(pairHead(args)):
        name, value = listElements(p) # each arg MUST be a pair
        names.append(strictValue(name))
        values.append(value)
    env = Environment([], [], env) # first make a dummy extension
    # use dummy extension as evaluation environment for function constructors
    for name, value in izip(names, strictValues(env, *values)):
        # then fill the extension with the name/function pairs
        # each function then has a reference to all the names
        env.define(name, value)
    # return the expr to evaluate with the new environment
    return pairHead(pairTail(args)), env


class Applicable(object):
    _eval = None
    def __init__(self, args, env):
        self.params, self.body = listElements(args)
        self.params = strictValue(self.params)
        self.body = strictValue(self.body)
        self.env = env
    def __call__(self, args, env):
        abstract
    def __repr__(self): # for pretty printing
        return "%s(%s, %s)" % (self.__class__.__name__,
                                   self.params, self.body)
    def _makeBodyEnv(self, args, env):
        result = tuple(self._matchArgsToParams(args, env))
        if result:
            params, args = zip(*result)
        else:
            params, args = (), ()
        return Environment(params, args, self.env)
#        return Environment(listElements(self.params),
#                           (self._eval(arg, env) for arg in
#                            listElements(args)),
#                           self.env)
    def _evalArg(self, arg, env):
        expr, env = self._eval(makeList(makeList(Symbol("quote"), arg), env),
                               env)
        return eval(expr, env)
    def _matchArgsToParams(self, args, env):
        params = self.params
        while isPair(params):
            param = strictValue(pairHead(params))
            yield param, self._evalArg(pairHead(args), env)
            params = pairTail(params)
            args = pairTail(args)
        params = strictValue(params)
        if not isNil(params): # rest parameter
#            print args
            yield params, makeList(*tuple(self._evalArg(arg, env)
                                          for arg in listElements(args)))
            args = nil
        assert isNil(args)
    # create an argument parser that understands things like &rest, &optional
    # and/or pattern-matching on list structure (dots or internal lists)
    # could also do keyword arguments, but they're not important right now
    def _matchArgsToParams_Future(self, args, env):
        params = self.params
        arg = args
        if not isNil(args):
            arg = pairHead(args)
        while isPair(params):
            # annotated param example: (a b (c (default 3)) . rest)
            evalFunc = self._eval
            param = pairHead(params)
            if isPair(param):
                param, evalFunc = listElements(param)
                evalFunc = evalStrict(evalFunc, self.env)
            yield param, eval(*evalFunc(makeList(
                makeList(Symbol("quote"), arg), env), env))
            params = pairTail(params)
            if not isNil(args):
                args = pairTail(args)
                if not isNil(args):
                    arg = pairHead(args)
                else:
                    arg = nil
        if not isNil(params): # rest parameter
            yield params, makeList(*(self._eval(arg, env)
                                     for arg in listElements(args)))
        assert isNil(args)



class Macro(Applicable):
    _eval = staticmethod(applyEvalSyntax)
    def __call__(self, args, env):
        # process the template
        result = evalStrict(self.body, self._makeBodyEnv(args, env))
        # then return the evaluated template back into the outer code
        return result, env

class Func(Applicable):
    def __call__(self, args, env):
        return self.body, self._makeBodyEnv(args, env)

class Proc(Applicable):
    def __init__(self, args, env):
        self.params = pairHead(args)
        self.body = pairTail(args)
        assert isPair(self.body)
        self.env = env
    def __call__(self, args, env):
        return evalSequence(self.body, self._makeBodyEnv(args, env))

# shouldn't have to define any function types here
# instead provide make-fn-type and make-proc-type taking a name (ie. strict-fn)
# and an evaluation func for the parameter evaluation strategy for it to use
# (ie. eval-strict or eval-lazy, or some user-made eval)

class StrictMacro(Macro):
    _eval = staticmethod(applyEvalStrict)


class StrictFunc(Func):
    _eval = staticmethod(applyEvalStrict)


class LazyFunc(Func):
    _eval = staticmethod(applyEvalLazy)


# treats arguments like a macro, but it's called during runtime and returns
# a result just like a function, so it's not for transforming code like a macro
# but with optimized compilation it could serve the same purpose
# actually this won't work correctly... need lisp-style macros, damn
#@topLevel("syntax-fn")
#def syntaxFunc(args, env):
#    return SyntaxFunc(args, env), None

@topLevel("macro")
def macro(args, env):
    return Macro(args, env), env

@topLevel("strict-macro")
def macro(args, env):
    return StrictMacro(args, env), env

@topLevel("strict-fn")
def strictFunc(args, env):
    return StrictFunc(args, env), None

@topLevel("lazy-fn")
def lazyFunc(args, env):
    return LazyFunc(args, env), None
