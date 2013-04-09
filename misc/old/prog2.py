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

from functools import partial


def add2(a, b):
    return evall(a) + evall(b)

def add3(a, b, c):
    return (add2, (add2, a, b), c)


expr = ((add3, 1, 2), 3)


def getArgCount(func):
    if isinstance(func, partial):
        return func.func.func_code.co_argcount - len(func.args)
    return func.func_code.co_argcount

def head(ex):
    assert isinstance(ex, tuple)
    return ex[0]

def tail(ex):
    assert isinstance(ex, tuple)
    return ex[1:]

# recursive definitions

def ev(ex): # eval
    if not isinstance(ex, tuple):
        return ex
    return ap(head(ex), tail(ex))

def ap(func, rest): # apply
    func = ev(func)
    if len(rest) < getArgCount(func):
        return partial(func, *rest)
    return ev(func(*rest))

# minimally recursive

def evit(ex, stack=[]):
    if isinstance(ex, tuple):
        while True:
            while True:
                stack.append(tail(ex))
                ex = head(ex)
                if not isinstance(ex, tuple):
                    break
#            yield ex
            while stack:
                args = stack.pop()
                if len(args) < getArgCount(ex):
                    ex = partial(ex, *args)
                else:
                    ex = ex(*args)
                    if isinstance(ex, tuple):
                        break
            if not isinstance(ex, tuple):
                break
    return ex


def eviter(ex, stack=[]):
    if isinstance(ex, tuple):
        while True:
            while True:
                stack.append(tail(ex))
                ex = head(ex)
                if not isinstance(ex, tuple):
                    break
            yield ex
            while stack:
                args = stack.pop()
                if len(args) < getArgCount(ex):
                    ex = partial(ex, *args)
                    yield ex
                else:
                    ex = ex(*args)
                    yield ex
                    if isinstance(ex, tuple):
                        break
            if not isinstance(ex, tuple):
                break
    yield ex

def evall(ex, stack=[]):
    for r in eviter(ex, stack):
        pass
    return r
