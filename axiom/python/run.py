#! /usr/bin/env python

from interpret import Interpreter, repl, DebugTracer
import sys

lastArgs = []
def run(): runWith(lastArgs)

def runWith(args=[]):
    global mod, interp, tracer, lastArgs
    lastArgs = args
    mod = None
    interp = Interpreter()
    tracer = DebugTracer(interp)
    if len(args) > 1: mod = interp.importModule(args[1], tracer, False)
    repl(interp, tracer, mod)

def trace(index=None):
    global tracer
    if index is None: tr = tracer
    else: tr = tracer.history[index]
    tr.walk()

def bt(out=sys.stdout):
    if interp.curBackTrace is None: print>>out, 'no backtrace is available'
    else:
        for tracer in interp.curBackTrace:
            for x in reversed(tracer.history): print>>out, x.show()

def btf(fileName='backtrace.txt'): bt(open(fileName, 'w'))

if __name__ == '__main__': runWith(sys.argv)
