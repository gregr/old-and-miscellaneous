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

from pygame.event import get_grab, set_grab
from pygame.mouse import set_visible, set_pos


BUTTON_LEFT = 1
BUTTON_MIDDLE = 2
BUTTON_RIGHT = 3
WHEEL_UP = 4
WHEEL_DOWN = 5


class CursorHandler(object):
    """Mouse input handler for user interfaces with cursors

    A cursor must expose absPos, move, buttonDown() and buttonUp().
    """
    
    def __init__(self, cursor, visible=False):
        set_visible(visible)
        self.cursor = cursor
        self.absPos = (0,0)
        self.ignoreMove = False
        self._setDefaultButtonValues()
    def onMove(self, event):
        if self.ignoreMove:
            self.ignoreMove = False
        else:
            self.absPos = event.pos
            self.cursor.move(event.rel)
    def onButtonUp(self, event):
        if get_grab():
            set_grab(False)
            self._synch()
        self.cursor.buttonUp(event.button)
    def onButtonDown(self, event):
        if not get_grab():
            set_grab(True)
        self.cursor.buttonDown(event.button)
    def show(self):
        set_visible(True)
    def hide(self):
        set_visible(False)
    def _synch(self):
        self.ignoreMove = True
        set_pos(self.cursor.pos) # this generates an unnecessary move-event
    def _setDefaultButtonValues(self):
        self.cursor.setButtonValues(BUTTON_LEFT, BUTTON_RIGHT, BUTTON_MIDDLE,
                                    WHEEL_UP, WHEEL_DOWN)
