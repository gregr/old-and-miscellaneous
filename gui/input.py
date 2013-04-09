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

from uriel.util.tuplevector import add as addTuples, sub as subTuples
from weakref import ref


class Listener(object):
    """An event listener that interacts with a user interface"""

    def __init__(self, owner):
        """Create a listener owned by an element."""
        self._ownerRef = ref(owner)
        self._focusRef = self._ownerRef

    def giveFocus(self, element):
        pass

    def takeFocus(self, element):
        pass

    def _getOwner(self):
        owner = self._ownerRef()
        assert owner is not None
        return owner

    def _getFocus(self):
        focus = self._focusRef()
        if focus is None:
            self._resetFocus()
            focus = self._focusRef()
        return focus

    def _resetFocus(self):
        self.focus = self.owner

    def _setFocus(self, focus):
        f = self.focus
        if f is not focus:
            self.takeFocus(f)
            self.giveFocus(focus)
            self._focusRef = ref(focus)

    owner = property(_getOwner)
    focus = property(_getFocus, _setFocus)


class Cursor(Listener):
    """A mouse listener"""

    def __init__(self, owner):
        """Create a cursor within an owning element."""
        Listener.__init__(self, owner)
        self._domain = self.owner.region
        self._curPos = owner.region.pos
        self._oldPos = self._curPos

    def setButtonValues(self, primary, secondary, tertiary,
                        scrollUp, scrollDown):
        self.buttonPrimary = primary
        self.buttonSecondary = secondary
        self.buttonTertiary = tertiary
        self.buttonScrollUp = scrollUp
        self.buttonScrollDown = scrollDown
        self.pressed = {primary:None, secondary:None, tertiary:None,
                        scrollUp:None, scrollDown:None}

    def giveFocus(self, element):
        """Signal a cursor gain event."""
        element.onCursorGain(self)

    def takeFocus(self, element):
        """Signal a cursor lose event."""
        element.onCursorLose(self)

    def move(self, rel):
        """Move the cursor."""
        x, y = addTuples(self.pos, rel)
        left, top = self._domain.pos
        right, bottom = self._domain.extent
        if x < left: x = left
        elif x >= right: x = right - 1
        if y < top: y = top
        elif y >= bottom: y = bottom - 1
        pos = (x, y)
        self.pos = pos
        # signal events
        focus = self.focus
        focus.onCursorMove(self)
        next = self.owner.getAtPos(pos)
        self.focus = next

    def buttonDown(self, button):
        """Signal a button down event."""
        focus = self.focus
        focus.moveToTop()
        focus.onButtonDown(self, button)
        self.pressed[button] = focus

    def buttonUp(self, button):
        """Signal a button up event."""
        pressed = self.pressed[button]
        if pressed is not None:
            pressed.onButtonUp(self, button)
            self.pressed[button] = None
#        focus = self.focus
        # might not make this decision at all...
        # if things shrink the domain, they should be smart enough to restore
        # it themselves, too
#        if not focus.onButtonUp(self, button):
#            self._domain = self.owner.region # release any restrictions
#        focus.onButtonUp(self, button):

    def restrictTo(self, region): # might as well just have domain set directly
        """Restrict movement to this region."""
        self._domain = region

    def freeRestriction(self):
        """Free any movement restriction."""
        self._domain = self.owner.region

    def _resetFocus(self):
        self.focus = self.owner.getAtPos(self.pos)

    def _getPos(self):
        return self._curPos

    def _setPos(self, pos):
        self._oldPos = self._curPos
        self._curPos = pos

    def _getRel(self):
        return subTuples(self._curPos, self._oldPos)

    pos = property(_getPos, _setPos)
    rel = property(_getRel)


class Caret(Listener):
    """A keyboard listener"""

    def __init__(self, owner):
        """Create a caret owned by an element."""
        Listener.__init__(self, owner)

    def giveFocus(self, element):
        """Signal a caret gain event."""
        element.onCaretGain(self)

    def takeFocus(self, element):
        """Signal a caret lose event."""
        element.onCaretLose(self)

    def keyDown(self, key):
        """Signal a key down event."""
        self.focus.onKeyDown(self, key)

    def keyUp(self, key):
        """Signal a key up event."""
        self.focus.onKeyUp(self, key)
