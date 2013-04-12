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

"""Commonly-used utilities""" # not so much anymore

# from copy import copy

# class FunctionMap(object):
#     """A mechanism for calling functions by name

#     This facilitates the use of data to describe method calls such as in
#     the case of performing remote procedure calls.
#     """
#     _KeymapType = SymbolicEnum

#     def __init__(self):
#         self.map = {}
#         self.keys = _KeymapType()

#     def __call__(self, key, *args, **kwargs):
#         """Call the function indexed by the given key."""
#         return self.map[key](*args, **kwargs)

#     def expose(self, func):
#         """Add a function to the internal map and create a key for it."""
#         name = func.func_name
#         if name not in vars(self.keys):
#             self.keys.addName(name)
#         key = getattr(self.keys, name)
#         self.map[key] = func
#         return func

#     def copy(self):
#         """Create a copy of this map.
        
#         Extending a copy of the map will allow polymorphic behavior
#         without affecting the original.
#         """
#         newMap = self.__class__()
#         newMap.map = self.map.copy()
#         newMap.keys = copy(self.keys)
#         return newMap

# class EnumeratedFunctionMap(FunctionMap):
#     """A FunctionMap which uses enumerated key-naming

#     Enumerated naming may be a useful network optimization when using
#     this class to implement remote procedure calls.
#     """
#     _KeymapType = Enum
