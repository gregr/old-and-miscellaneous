# todo:
# add better errors and checking for debugging,
# better pretty-printed function names,
# list and tuple syntax (maybe comma too)
# keyword arguments (record syntax for tuples?)
# unary ops, infix ops (= with negative precedence whereas ordinary function app is 0 precedence), infix op sections,
# eval, other primitives and convenient define,
# let, where (as part of = macro), case and pattern-matching?
# syntax-rules, errors (exceptions), concurrency, compilation
# garbage collection... oh boy

from data import symbol
from core import strictValue, analyze, Operator, actualValue
from syntax import parseStream
from prim import rootEnv

def evalExpr(ex, env):
#    print "expr:", ex
    return strictValue(analyze(ex), env)

testOps = {
    '$': Operator("prefix", "left", 12),
    '~': Operator("prefix", "left", 12),
    '!': Operator("prefix", "left", 12),
    '?': Operator("postfix", "right", 12),
    '.': Operator("infix", "left", 14),
    '++': Operator("infix", "right", 7),
    '**': Operator("infix", "left", 9),
    '*': Operator("infix", "left", 8),
    '/': Operator("infix", "left", 8),
    '%': Operator("infix", "left", 8),
    '+': Operator("infix", "left", 7),
    '-': Operator("infix", "left", 7),
    '<': Operator("infix", "left", 6),
    '>': Operator("infix", "left", 6),
    '<=': Operator("infix", "left", 6),
    '>=': Operator("infix", "left", 6),
    '==': Operator("infix", "left", 6),
    '===': Operator("infix", "left", 6),
    '!=': Operator("infix", "left", 6),
    '<>': Operator("infix", "left", 6),
    '&&': Operator("infix", "left", 5),
    '||': Operator("infix", "left", 4),
    ':': Operator("infix", "right", 2), # yeah, I'm just guessing at this point
    '@': Operator("infix", "left", 1),
    '=': Operator("infix", "right", -100), # define
    '$=': Operator("infix", "right", -100), # strict define
    ':=': Operator("infix", "right", -99), # set
    '$:=': Operator("infix", "right", -99), # strict set
    '->': Operator("infix", "right", -98), # lambda
}

def evalStream(stream, env=rootEnv, ops=testOps):
    for expr in parseStream(stream, env, ops):
        yield evalExpr(expr, env)

def evalFile(fileName, ops=testOps):
    return evalStream(open(fileName), rootEnv, ops)

def go(stream):
    for x in stream:
        pass
