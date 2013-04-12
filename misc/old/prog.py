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

from operator import *
from functools import partial

def makeOp(op):
    return lambda a: lambda b: op(a, b)

addl = makeOp(add)
divl = makeOp(div)

expr = (divl, (addl, 2, 5), (addl, 3, 4))


def head(ex):
    assert isinstance(ex, tuple)
    return ex[0]

def tail(ex):
    assert isinstance(ex, tuple)
    return ex[1:]

def ev(ex): # eval
    if not isinstance(ex, tuple):
        return ex
    return ap(head(ex), tail(ex))

def ap(func, rest): # apply
    func = ev(func)
    arg = ev(head(rest))
    result = func(arg)
    more = tail(rest)
    if more:
        result = ap(result, more)
    return result
