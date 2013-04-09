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

from uriel.util.misc import doNothing
from collections import defaultdict


class Presentation(object):
    """A null baseclass for presentations."""

    def __init__(self, *args, **kwargs):
        pass
    def __getattr__(self, name):
        return doNothing
    def minSize(self):
        return (0,0)


class ThemeMap(defaultdict):
    def __init__(self, default, defaultKey=None):
        self.defaultKey = defaultKey
        self.__setitem__(defaultKey, default)
    def __missing__(self, key):
        return self.__getitem__(self.defaultKey)
