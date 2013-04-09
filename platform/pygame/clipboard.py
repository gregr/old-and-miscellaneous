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

import pygame
from pygame.locals import * # SCRAP_TEXT, SCRAP_BMP, SCRAP_PPM, SCRAP_PBM?

def types():
    return pygame.scrap.get_types()

def isOwned():
    return pygame.scrap.lost()

def containsString():
    return pygame.scrap.contains(SCRAP_TEXT)

def containsImage():
    return pygame.scrap.contains(SCRAP_BMP)

def getString():
    return pygame.scrap.get(SCRAP_TEXT)

def getImage():
    return pygame.scrap.get(SCRAP_BMP)

def putString(s):
    pygame.scrap.put(SCRAP_TEXT, s)

def putImage(s):
    pygame.scrap.put(SCRAP_BMP, s)
