from data import isSymbol, isUnique, isTuple, isPair
from core import actualValue, pairListIter, pairIter

def prettyPairList(p, pairOp=" : "):
    try:
        return "[%s]" % " ".join([pretty(x) for x in pairListIter(p)])
    except: # todo: error type?
        return "(%s)" % pairOp.join([pretty(x) for x in pairIter(p)])

def prettyTuple(t):
    return "{%s}" % " ".join([pretty(x) for x in t])

def prettyNamed(s):
    return s.name

def pretty(x):
    x = actualValue(x)
    if isPair(x):
        return prettyPairList(x)
    elif isTuple(x):
        return prettyTuple(x)
    elif isSymbol(x) or isUnique(x):
        return prettyNamed(x)
    elif isinstance(x, str):
        return '"%s"' % str(x)
    return repr(x)
