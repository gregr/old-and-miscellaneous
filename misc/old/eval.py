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

from itertools import izip

nil = None

def makePair(h, t):
    return [h, t]

def pairHead(p):
    return p[0]

def pairTail(p):
    return p[1]

def setPairHead_(p, val):
    p[0] = val

def setPairTail_(p, val):
    p[1] = val


def makeList(*args):
    l = nil
    for a in reversed(args):
        l = makePair(a, l)
    return l

def listElements(l):
    while l is not nil:
        h = pairHead(l)
        l = pairTail(l)
        yield h


# environment frames
def makeFrame(variables, values):
    return makePair(variables, values)

def frameVariables(frame):
    return pairHead(frame)

def frameValues(frame):
    return pairTail(frame)

def addBindingToFrame_(frame, var, val):
    setPairHead_(frame, makePair(var, pairHead(frame)))
    setPairTail_(frame, makePair(val, pairTail(frame)))


# environments
theEmptyEnvironment = None

def enclosingEnvironment(env):
    return pairTail(env)

def firstFrame(env):
    return pairHead(env)

def extendEnvironment(env, variables, values):
    assert len(variables) == len(values)
    return makePair(makeFrame(variables, values), env)

def _lookupVariableValue(env, variable):
    while True:
        assert env is not theEmptyEnvironment
        frame = firstFrame(env)
        vars = frameVariables(frame)
        vals = frameValues(frame)
        while vars is not nil:
            var = pairHead(vars)
            if var == variable:
                return vals
            vars = pairTail(vars)
            vals = pairTail(vals)
        env = enclosingEnvironment(env)

def lookupVariableValue(env, variable):
    return pairHead(_lookupVariableValue(env, variable))

def setVariableValue_(env, variable, value):
    setPairHead_(lookupVariableValue(env, variable), value)

def defineVariable_(env, variable, value):
    frame = firstFrame(env)
    vars = frameVariables(frame)
    vals = frameValues(frame)
    while vars is not nil:
        var = pairHead(vars)
        if var == variable:
            setPairHead_(vals, value)
            return
        vars = pairTail(vars)
        vals = pairTail(vals)
    addBindingToFrame_(frame, variable, value)

userInitialEnvironment = extendEnvironment(theEmptyEnvironment, [], [])
