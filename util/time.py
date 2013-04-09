# Copyright (C) 2008 Gregory L. Rosenblatt
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

"""Timing utilities."""

import calendar
import time

class Timer(object):
    """A callable object that tracks the time elapsed between calls"""
    def __init__(self, clock=None):
        """Create a timer based on the given clock function."""
        if clock is None:
            clock = time.clock
        self.current = clock()
        self.clock = clock

    def __call__(self):
        """Compute the time elapsed since the last call and reset the timer."""
        next = self.clock()
        delta = next - self.current
        self.current = next
        return delta


def localToUtc(tt):
    """Convert a local time tuple to UTC time."""
    return time.gmtime(time.mktime(tt))

def utcToLocal(tt):
    """Convert a UTC time tuple to local time."""
    return time.localtime(calendar.timegm(tt))
