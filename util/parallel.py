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

"""Parallel-processing primitives"""

class ProcessGroup(object):
    """A group that oversees the advancement of processes in parallel"""

    def __init__(self):
        """Create an empty group."""
        self._processes = set()
        self._addedProcesses = set()
        self._removedProcesses = set()
        self.updater = _processUpdater(self._processes,
                                     self._addedProcesses,
                                     self._removedProcesses)

    def __iter__(self):
        """Return an iterator that will advance all processes in parallel."""
        return self.updater

    def add(self, process):
        """Add a process to the group."""
        self._addedProcesses.add(process)

    def remove(self, process):
        """Remove a process from the group."""
        self._removedProcesses.add(process)


class Process(object):
    """A managed iterative process"""

    __slots__ = ["process", "group"]

    def __init__(self, process, group):
        """Create a managed process and add it to the given group."""
        self.group = group
        self.process = process
        group.add(process)

    def __del__(self):
        """Remove the process from its group."""
        self.group.remove(self.process)


def _processUpdater(processes, addedProcesses, removedProcesses):
    """Advance each of the given processes once every update."""
    while True:
        if addedProcesses:
            processes |= addedProcesses
            addedProcesses.clear()
        if removedProcesses:
            processes -= removedProcesses
            removedProcesses.clear()
        ps = iter(processes)
        while True:
            try:
                for p in ps:
                    p.next()
                break
            except StopIteration:
                removedProcesses.add(p)
        del p   # don't prevent garbage collection
        del ps
        yield None
