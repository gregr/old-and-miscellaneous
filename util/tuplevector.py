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

"""Basic operations for tuple-based vectors"""

from itertools import izip


def add(v, vv):
    """Return the sum of two vectors."""
    return tuple(i+ii for i, ii in izip(v, vv))

def sub(v, vv):
    """Return the difference of two vectors."""
    return tuple(i-ii for i, ii in izip(v, vv))

def neg(v):
    """Return v scaled by -1."""
    return tuple(-i for i in v)

def scale(v, s):
    """Return v scaled by scalar s."""
    return tuple(i*s for i in v)

def mod(v, s):
    """Return a vector with each of v's elements modulo s."""
    return tuple(i%s for i in v)

def floorDiv(v, s):
    """Return v floor-divided by scalar s."""
    return tuple(i//s for i in v)

def toInt(v):
    """Return a vector with each of v's elements cast to int."""
    return tuple(int(i) for i in v)

def toFloat(v):
    """Return a vector with each of v's elements cast to float."""
    return tuple(float(i) for i in v)

def reversedCmp(v, vv):
    """Compare two vectors from the last element to the first."""
    return cmp(tuple(reversed(v)), tuple(reversed(vv)))
