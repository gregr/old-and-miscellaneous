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

import pygame

depth = 0 # a value of zero lets the system choose an appropriate depth
_flags = 0

# doublebuf is also recommended if opengl or hwsurface is set
def changeFlags(flags, resizable=None, fullscreen=None, noframe=None,
                doublebuf=None, hwsurface=None, opengl=None):
#    global _flags
    choices = {pygame.RESIZABLE:resizable,
               pygame.FULLSCREEN:fullscreen,
               pygame.NOFRAME:noframe,
               pygame.DOUBLEBUF:doublebuf,
               pygame.HWSURFACE:hwsurface,
               pygame.OPENGL:opengl}
    for key, value in choices.iteritems():
        if value == True:
            flags |= key
        elif value == False:
            flags &= ~key
        else:
            assert value == None, "argument choices are True, False, or None"
    return flags

def setFlags(*args, **kwargs):
    global _flags
    _flags = changeFlags(_flags, *args, **kwargs)

def create(resolution):
    pygame.display.set_mode(resolution, _flags, depth)

def setIcon(surface): # if used, it should be called before create()
    pygame.display.set_icon(surface)

def setCaption(caption):
    pygame.display.set_caption(caption)

def getCaption():
    return pygame.display.get_caption[0]
