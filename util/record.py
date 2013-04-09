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

"""Utilities for literal object creation."""

class Record(object):
    """An anonymous record"""
    def __init__(self, **attributes):
        """Create a record with the given attributes."""
        self.__dict__.update(attributes)

    def __repr__(self):
        return (self.__class__.__name__ + "(%s)" % ", ".join(
            "%s=%s" % (k, repr(v)) for k,v in vars(self).iteritems()))


class Enum(Record):
    """A record containing enumerated symbols"""
    def __init__(self, symbols):
        """Enumerated the given symbols."""
        Record.__init__(self, **dict((n, v) for v, n in enumerate(symbols)))

    def _addSymbol(self, symbol):
        """Append a symbol with the next available enumerated value."""
        setattr(self, symbol, len(vars(self).keys()))


class ReflexiveEnum(Record):
    """A record containing reflexive fields."""
    def __init__(self, symbols):
        """Create a record of the given symbols."""
        Record.__init__(self, **dict((symbol, symbol) for symbol in symbols))

    def _addSymbol(self, symbol):
        """Append a symbol to as a field."""
        setattr(self, symbol, symbol)


def loadConfig(fileName, globals=None):
    """Create a record containing bindings defined in a python file."""
    cfg = Record()
    locals = cfg.__dict__
    if globals is None:
        globals = locals
    execfile(fileName, globals, locals)
    return cfg
