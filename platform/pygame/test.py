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

import display
from event import Dispatcher
import pygame
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("test")

resolution = (400, 400)

d = Dispatcher()

def printEvent(e):
    print e

def paintRect(e):
    print e
    pygame.display.get_surface().fill((100, 100, 150),
                                      pygame.Rect(e.pos, (10, 10)))

d.addHandlers({pygame.MOUSEBUTTONDOWN:paintRect,
               pygame.MOUSEBUTTONUP:printEvent,
               pygame.VIDEORESIZE:lambda e: display.create(e.size)})

display.setFlags(resizable=True)
display.setCaption("pygame-platform test")
display.create(resolution)

d.start(pygame.display.flip)
