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

from uriel.gui.output import ThemeMap, Presentation
from uriel.gui.control import CommandButton, Switch
from uriel.gui.base import Root, Element
from mouse import CursorHandler
import display
from event import Dispatcher
import pygame
import logging
from weakref import WeakKeyDictionary


def drawRect(color, pos, size):
    pygame.display.get_surface().fill(color, pygame.Rect(pos, size))


class DrawnPresentation(Presentation):
    heightChanged = True
    def __init__(self, element):
        self.element = element
    def onHeightChange(self):
        # useful when not drawing directly from the top down anymore
        # and recomputing element heights only as necessary
        # for the sorted-state rendering algorithm
        DrawnPresentation.heightChanged = True
        # that algorithm will recompute heights whenever this is True
        # and then set this to False


class ColorPresentation(DrawnPresentation):
    def __init__(self, *args, **kwargs):
        DrawnPresentation.__init__(self, *args, **kwargs)
    def drawColored(self, color):
        element = self.element
        if element.visible:
            drawRect(color, element.region.pos, element.region.size)


class ButtonPresentation(ColorPresentation):
    def __init__(self, *args, **kwargs):
        ColorPresentation.__init__(self, *args, **kwargs)
    def draw(self):
        if self.element.pressed:
            color = (100, 100, 150)
        else:
            color = (200, 200, 255)
        self.drawColored(color)

class SwitchPresentation(ColorPresentation):
    def __init__(self, *args, **kwargs):
        ColorPresentation.__init__(self, *args, **kwargs)
    def draw(self):
        self.drawColored(self.element.state)

CommandButton.themes = ThemeMap(ButtonPresentation)
Switch.themes = ThemeMap(SwitchPresentation) # only if states are colors...

class PygameRoot(Root):
    def __init__(self, size):
        display.setFlags(resizable=True)
        display.setCaption("pygame-platform test")
        display.create(size)
        Root.__init__(self, size)
        # mouse-visibility true only until it can be drawn with the renderer
        self.mouse = CursorHandler(self.cursor)#, True)
    def onSize(self, size):
        display.create(size)
        Root.onSize(self, size)

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger("test")

resolution = (400, 400)

root = PygameRoot(resolution)

def printCommand():
    print "button clicked"

button = CommandButton(printCommand, root, (10, 10), (50, 50))
switch = Switch([(255, 0, 0), (255, 255, 0), (0, 255, 0),
                 (0, 255, 255), (0, 0, 255), (255, 0, 255), (255, 255, 255)],
                root, (40, 40), (80, 80))

d = Dispatcher()
d.addHandlers({pygame.MOUSEBUTTONDOWN:root.mouse.onButtonDown,
               pygame.MOUSEBUTTONUP:root.mouse.onButtonUp,
               pygame.MOUSEMOTION:root.mouse.onMove,
               pygame.VIDEORESIZE:lambda e:root.resize(e.size)})

def draw():
    # should fill display with a clearing color first each frame
    drawRect((0,0,0), root.region.pos, root.region.size)
    for element in root: # should eventually use sorted rendering
        element.presentation.draw()
    drawRect((128, 128, 128), root.cursor.pos, (10, 10))
    pygame.display.flip()

try:
    d.start(draw)
    pass
except:
    pygame.quit()
    raise
