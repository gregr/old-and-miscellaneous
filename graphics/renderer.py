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

#from collections import defaultdict
from itertools import chain

# example state variables:
# texture
# color
# z-depth
# clipping region

class StateOps(object):
    def __init__(self):
        self.map = {}
        self.changed = True
    def add(self, state, op):
        try:
            ops = self.map[state]
        except KeyError:
            ops = []
            self.map[state] = ops
            self.changed = True
        ops.append(op)
    def remove(self, state, op):
        ops = self.map[state]
        ops.remove(op)
        if not ops:
            del self.map[state]
            found = False
            for i, p in enumerate(self._ordered):
                if p[0] == state:
                    found = True
                    break
            if found:
                del self._ordered[i]
    def _getOrdered(self):
        if self.changed:
            self._ordered = sorted(self.map.iteritems())
            self.changed = False
        return self._ordered
    ordered = property(_getOrdered)


def makeOpSequence(makeStateTransitionOps, orderedStateMap, prevState):
    for state, ops in orderedStateMap:
        for op in chain(makeStateTransitionOps(prevState, state), ops):
            yield op
        prevState = state


def opSequenceAndFinalState(makeStateTransitionOps, stateMap, initState):
    stateMap = sorted(stateMap.iteritems())
    return (makeSequence(makeStateTransitionOps, stateMap, initState)
            ,stateMap[-1])


def render(makeTransitionOps, stateMap, initState):
    ops, finalState = opSequenceAndFinalState(makeTransitionOps,
                                              stateMap, initState)
    for op in ops:
        op()
    return finalState
