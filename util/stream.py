# Copyright (C) 2007-2008 Gregory L. Rosenblatt
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

"""Utilities for extending iterables"""

class _Stream(object):
    """An adapter to give put-back behavior to iterables"""
    def __init__(self, itr):
        """Create a stream that wraps the given iterable."""
        self.itr = itr
        self.buffer = []

    def __iter__(self):
        return self

    def next(self):
        """Return the next available item."""
        if self.buffer:
            return self.buffer.pop()
        return self.itr.next()

    def put(self, item):
        """Put the given item into the stream for later retrieval."""
        self.buffer.append(item)

    def empty(self):
        """Determine if the stream is empty."""
        if self.buffer:
            return False
        try:
            self.put(self.stream.next())
        except StopIteration:
            return True
        return False

def Stream(s):
    """Intelligently wrap an iterable with a Stream."""
    if isinstance(s, _Stream):
        return s
    else:
        return _Stream(s)

# class InputReader(object):
#     """A special adapter for reading data from file-like objects."""
#     def __init__(self, io):
#         self.io = io
#         self._readNextLine()
#     def next(self):
#         ch = self.peek()
#         self.current += 1
#         return ch
#     def prev(self):
#         assert self.current > 0
#         self.current -= 1
#     def peek(self):
#         if self.current >= len(self.line):
#             self._readNextLine()
#         return self.line[self.current]
#     def _readNextLine(self):
#         line = self.io.readline()
#         if not line:
#             raise EOFError
#         self.line = line
#         self.current = 0
