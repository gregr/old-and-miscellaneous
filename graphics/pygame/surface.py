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
from pygame.locals import *

def fill(target, color, rect=None): # flags?
    return target.fill(color, rect)

def color(surface, rgba): # probably not useful
    return surface.map_rgb(rgba)

rect = pygame.Rect
flip = pygame.transform.flip
resize = pygame.transform.scale
rotate = pygame.transform.rotate
toString = pygame.image.tostring
fromString = pygame.image.fromString
# toString format strings: RGB, RGBX, RGBA, ARGB, RGBA_PREMULT, ARGB_PREMULT

def doClipped(surface, clipRect, op):
    """Perform the given op within a clipped region of a surface."""
    lastRect = surface.get_clip()
    surface.set_clip(clipRect)
    try:
        op()
    finally:
        surface.set_clip(lastRect)

def create(size, matchSurface=None): # optional format matching
    """Create an empty surface of the given size.

    If a surface is provided, the new surface's formatting will match it.
    """
    if surfaceToMatch is not None:
        return pygame.Surface(size, matchSurface.get_flags(), matchSurface)
    return pygame.Surface(size)

# use get_flags() to determine if RLEACCEL is intended in save/copy?
def copy(source, target):
    """Copy the source surface data directly to the target.

    The source's upper left corner will correspond with the target's.
    Transparency information is preserved.
    """
    ck = source.get_colorkey()
    if ck:
        source.set_colorkey()
        target.blit(source, (0, 0))
        source.set_colorkey(ck, RLEACCEL)
        target.set_colorkey(ck, RLEACCEL)
    else:
        target.blit(source, (0, 0))

# useful for creating copies with power-of-two dimensions for opengl textures
def copied(source, size):
    """Create a surface containing a copy of the source surface.

    The new surface will be of the size specified with the source copied at the
    upper left.
    """
    copied = makeEmpty(size, source)
    copy(source, copied)
    return copied

def saveToFile(surface, filePath):
    """Save the surface data to file."""
    ck = surface.get_colorkey()
    surface.set_colorkey()
    pygame.image.save(surface, filePath)
    if ck:
        surface.set_colorkey(ck, RLEACCEL)

# TODO: defer use of memoization/pooling for higher-level flexibility
def loadFromFile(filePath, colorKey=None):
    """Create a surface from the given image file.

    An optional color key may be specified directly or a value of True can be
    given, indicating the color key is encoded in the image itself at (0,0).
    """
    surface = pygame.image.load(filePath).convert()
    if colorKey:
        if colorKey is True:
            colorKey = surface.get_at((0, 0))
        surface.set_colorkey(colorKey, RLEACCEL)
    return surface

class Image(object):
    """An image described by a particular region of a surface"""
    def __init__(self, surface, region=None):
        """Create an image described by the region of the given surface.

        If region is None, the entire surface is used.
        """
        self.surface = surface
        if region is None:
            region = surface.get_rect()
        self.region = region

    def draw(self, target, pos):
        """Draw the image at a certain position on the target surface."""
        target.blit(self.surface, pos, self.region)
