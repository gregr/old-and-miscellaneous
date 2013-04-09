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

# use parallel.Process with this class's updater

class Agent(object):
    """A program executing in a simulation that receives percepts as input"""
    def __init__(self, program):
        self.program = program
        self.percepts = []
    def updater(self):
        """Create a process that continually executes the program."""
        return _agentUpdater(self.program, self.percepts)


def _agentUpdater(program, percepts):
    while True:
        if percepts:
            program(percepts)
            del percepts[:]
        yield None
