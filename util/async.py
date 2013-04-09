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

"""Asynchronous-programming primitives"""

from collections import deque
import types
import sys


class Scheduler(object):
    """An asynchronous task scheduler implemented with coroutines"""

    def __init__(self):
        """Create a new coroutine scheduler."""
        self.tasks = deque()

    def __iter__(self):
        """Create an iterator for the scheduler's main loop."""
        return _runTasks(self.tasks)

    def add(self, coroutine):
        """Add a coroutine to the task list."""
        self.tasks.append(_makeTask(coroutine))

    def run(self):
        """Run the main loop until all tasks are completed."""
        for result in iter(self): # simply a shorthand way of calling next()
            pass
        return result # this will be the result yielded by the final task


def _makeTask(coroutine, stack=(), inputValue=None, *excInfo):
    return (coroutine, stack, inputValue, excInfo)


def _runTasks(tasks):
    """Return a generator that implements the main loop logic."""
    while tasks:
        coroutine, stack, inputValue, excInfo = tasks.popleft()
        try:
            if excInfo: # propogate exception info
                result = coroutine.throw(inputValue, *excInfo)
            else: # inject actual input data
                result = coroutine.send(inputValue)
        except:
            if stack: # propogate the exception to the parent coroutine
                tasks.append(_makeTask(stack[0], stack[1], *sys.exc_info()))
                yield
                continue
            else: # if there is no parent, propogate it to the caller
                raise
        if isinstance(result, types.GeneratorType):
            # yielded to a new coroutine
            tasks.append(_makeTask(result, (coroutine, stack)))
            yield
        elif stack:
            # yielded data to be sent to the parent coroutine
            tasks.append(_makeTask(stack[0], stack[1], result))
            yield
        else:
            # yielded data without a parent, yield to the caller
            yield result
