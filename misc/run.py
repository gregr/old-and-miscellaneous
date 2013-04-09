#! /usr/bin/python

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

from pretty import pretty
from core import *
import operator

# should wrap up the core that will be exposed in its own environment
# then evaluate the prelude within that environment
# THEN create a user top-level environment that is a child of the core
# this way the user can mask, but not eliminate, core definitions
applyLessThan = topLevel("<")(makeStrictFunc(operator.lt))
applyGreaterThan = topLevel(">")(makeStrictFunc(operator.gt))
applyAdd = topLevel("+")(makeStrictFunc(operator.add))
applySub = topLevel("-")(makeStrictFunc(operator.sub))
applyMul = topLevel("*")(makeStrictFunc(operator.mul))
applyDiv = topLevel("/")(makeStrictFunc(operator.div))

topEnv = makeTopEnvironment()

from syntax import _parseProg
from StringIO import StringIO

# put the prelude in an external file, for the love of god
preludeText = """
(define fn strict-fn)
"""

preludeStream = StringIO(preludeText)

# need to implement def-macro (and def-fn etc.)
# to do this more easily, define backquote, comma, etc. (comma-at, comma-dot?)
# to do THIS, need to implement list and append (and splice?)
# other words: quasiquote, unquote, splice

# the lexer and syntaxer really need to be improved...
# implement make-fn-type after lexing -> syntax construction works


def installPrelude():
    preludeList = _parseProg(preludeStream,
                             processWhitespace=False, printExprs=False)
    evalStrict(*evalSequence(makeList(*preludeList), topEnv))

def runProgram(fileName):
    # process the prelude before running the program
    installPrelude()
    progStream = open(fileName)
    # now run the program
    exprList = _parseProg(progStream, processWhitespace=False, printExprs=False)
    result = evalStrict(*evalSequence(makeList(*exprList), topEnv))
    print pretty(result)

# when there is a better "read" function available, create a repl loop
# otherwise it will be too crappy
def _interact(prompt="> "):
    installPrelude()
    # while input stream isn't at EOF
    # read input
    # output = eval input, topEnv
    # pretty print output

if __name__ == "__main__":
    import sys
    assert len(sys.argv) > 1, "You need to supply the name of a program to run"
    runProgram(sys.argv[1])
