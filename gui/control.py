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

from base import Element


# would be nice to allow button-specific commands? maybe in another class
class Button(Element):
    """"""

    def __init__(self, *args, **kwargs):
        Element.__init__(self, *args, **kwargs)
        self._pressed = False
        self.released = True

    def onCursorGain(self, cursor):
        Element.onCursorGain(self, cursor)
        if not self.released:
            self.pressed = True

    def onCursorLose(self, cursor):
        Element.onCursorLose(self, cursor)
        if not self.released:
            self.pressed = False

    def onButtonDown(self, cursor, button):
        Element.onButtonDown(self, cursor, button)
        if button == cursor.buttonPrimary:
            self.pressed = True
            self.released = False

    def onButtonUp(self, cursor, button):
        Element.onButtonUp(self, cursor, button)
        if (button == cursor.buttonPrimary):
            self.released = True
            if self.pressed:
                self.pressed = False

    def onPressed(self):
        self.presentation.OnPressed()

    def onUnpressed(self):
        self.presentation.OnUnpressed()

    def _getPressed(self):
        return self._pressed

    def _setPressed(self, value):
        self._pressed = value
        if value:
            self.onPressed()
        else:
            self.onUnpressed()

    pressed = property(_getPressed, _setPressed)


class CommandButton(Button): # press and then release to execute command
    """"""
    def __init__(self, command, *args, **kwargs):
        Button.__init__(self, *args, **kwargs)
        self.command = command

    def onUnpressed(self):
        Button.onUnpressed(self)
        if self.released:
            self.command()


class DialButton(Element): # repeats if you hold it down
    """"""
    delay = 500
    period = 100
    def __init__(self, command, *args, **kwargs):
        Button.__init__(self, *args, **kwargs)
        self.command = command
        self.repeating = False
        self.timer = 0

    def onPressed(self):
        Button.onPressed(self)
        if self.released:
            self.command()
        self.repeating = True

    def onUnpressed(self):
        Button.onUnpressed(self)
        self.repeating = False
        if self.released:
            self.timer = 0

    def onUpdate(self, delta):
        if self.pressed:
            self.timer += delta
            if self.repeating:
                if self.timer > self.period:
                    self.timer -= self.period
                    self.command()
            elif self.timer > self.delay:
                self.timer -= self.delay
                self.repeating = True


from uriel.util.misc import doNothing
from itertools import cycle


class Switch(Element): # multi-state switch
    """"""
    def __init__(self, states, *args, **kwargs):
        Element.__init__(self, *args, **kwargs)
        self.command = doNothing
        self.states = cycle(states)
        self.onState()

    def onButtonDown(self, cursor, button):
        Element.onButtonDown(self, cursor, button)
        if button == cursor.buttonPrimary:
            self.onState()

    def onState(self):
        self.state = self.states.next()
        self.presentation.onState()
        self.command(self.state)
