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

import pygame

def systemFonts():
    return pygame.font.get_fonts()

# TODO: defer memoization
def create(name, size):
    """Load a TrueType font using pygame.

    If the given name doesn't correspond to a font file, it is assumed to be a
    system font name.  If there is no such system font, the default pygame font
    will be loaded.
    """
    try:
        return pygame.font.Font(name, size)
    except IOError: # name was not a valid file name
        return pygame.font.SysFont(name, size)

def wrapPredicate(font, width):
    """Create a predicate for use with word wrapping.

    The predicate determines whether a string of text rendered in the given
    font would be wider than the given width.
    """
    def shouldWrap(s):
        return font.size(s)[0] > width
    return shouldWrap

class Text(object):
    def __init__(self, value, font, color, bgcolor=None, antialias=True):
        self.font = font
        self.color = color
        self.bgcolor = bgcolor
        self.antialias = antialias
        self.value = value
    def draw(self, target, pos):
        target.blit(self._surface, pos)
    def _makeSurface(self):
        if self.bgcolor is None: # silly, can't pass None as last arg
            self._surface = self.font.render(self.value, self.antialias,
                                             self.color)
        else:
            self._surface = self.font.render(self.value, self.antialias,
                                             self.color, self.bgcolor)
    def _getValue(self):
        return self._value
    def _setValue(self, value):
        self._value = value
        self._makeSurface()
    value = property(_getValue, _setValue)
