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

from syntax import *
from core import *

# these are just place-holders...
# in the future these variables should actually look for the correct
# representations, which could be changed from these defaults
pairingToken = "."
beginExprToken = "("
endExprToken = ")"
_macros = [beginExprToken, endExprToken, pairingToken]

def prettyPair(tail):
    values = []
    while tail is not nil:
        if not isPair(tail):
            print "not pair:", tail
            values.append(pairingToken)
            values.append(pretty(tail))
            break
        head, tail = pairHead(tail), strictValue(pairTail(tail))
        values.append(pretty(head))
    return "%s%s%s" % (beginExprToken, " ".join(values), endExprToken)


def prettyString(s):
    return '"%s"' % (s.replace("\\", "\\\\")).replace("\"", "\\\"")


# somehow need access to the real macros list, and not this stand-in list
def prettySymbol(s):
    result = []
    for c in s.name:
        if c.isspace() or c in _macros:
            result.append("\\")
        result.append(c)
    return "".join(result)


def pretty(expr):
    expr = strictValue(expr)
    if isinstance(expr, Symbol):
        return prettySymbol(expr)
    elif isPair(expr):
        return prettyPair(expr)
    elif isinstance(expr, str):
        return prettyString(expr)
    elif isinstance(expr, Applicable):
        return "(%s %s %s)" % (expr.__class__.__name__,
                               pretty(expr.params), pretty(expr.body))
#        return repr(expr)
#        return repr(type(expr))
    else:
        return repr(expr)


import sys

def _interact():
    for t in parse(sys.stdin):
        print pretty(t)


if __name__ == "__main__":
    _interact()
