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

from base import Element, ElementContainer
from uriel.util.tuplevector import sub as subTuples
from uriel.util.misc import doNothing
from functools import partial


horizontal = 0
vertical = 1


def flipOrientation(orientation):
    if orientation == horizontal:
        return vertical
    return horizontal


def orientedOrder(orientation, major, minor):
    if orientation == 0:
        return (major, minor)
    return (minor, major)


def relPosOriented(index, element):
    return element.region.pos[index] - element.parent.region.pos[index]


class SliderBar(Element):
    def __init__(self, orientation, *args, **kwargs):
        self.command = doNothing
        self._orientation = orientation
        self._grabbed = False
        Element.__init__(self, *args, **kwargs)
    def onCursorMove(self, cursor):
        Element.onCursorMove(self, cursor)
        if self._grabbed:
            delta, value = constrainMovement(self._orientation[0],
                                             self, cursor.rel)
            self.move(delta)
            self.command(value, self.parent.interval)
    def onButtonDown(self, cursor, button):
        Element.onButtonDown(self, cursor, button)
        if button == cursor.buttonPrimary:
            cursor.restrictTo(self.parent.region)
            self._grabbed = True
    def onButtonUp(self, cursor, button):
        Element.onButtonUp(self, cursor, button)
        if button == cursor.buttonPrimary:
            cursor.freeRestriction()
            self._grabbed = False
    def _getValue(self):
        return relPosOriented(self._orientation, self)
    def _setValue(self, value):
        if value < 0:
            value = 0
        else:
            interval = self.parent.interval
            if value >= interval:
                value = interval - 1
        self._moveToValue(value)
    def _getRatio(self):
        pass
    def _setRatio(self, ratio):
        if (ratio < 0.0):
            ratio = 0.0
        elif ratio > 1.0:
            ratio = 1.0
        value = ratio * (self.parent.interval-1)
        self._moveToValue(value)
    value = property(_getValue, _setValue)
    ratio = property(_getRatio, _setRatio)
    def _constrainedMovement(self, rel):
        delta = rel[self._orientation]
        pos = self.value
        interval = self.parent.interval # should always be true
        #    interval = (self.parent.region.size[index] - self.region.size[index])
        diff = pos + delta
        if diff < 0:
            delta -= diff
        else:
            diff = interval - diff
            if diff < 0:
                delta += diff
                return orientedOrder(self._orientation, delta, 0), pos+delta
    def _moveToValue(self, value):
        self.move(orientedOrder(value - relPosOriented(self._orientation, self),
                                0))
        self.command(value, self.parent.interval)


class Slider(ElementContainer):
    def __init__(self, orientation, interval, *args, **kwargs):
        self.interval = interval
        self._orientation = orientation
        ElementContainer.__init__(self, *args, **kwargs)
        self._bar = SliderBar(orientation, parent=self)
        self.resize(self.region.size)
    def setInterval(self, interval):
        # TODO: make sure slider is long enough first...
        diff = self.interval - interval
        self.interval = interval
        size = self._bar.region.size[self._orientation]+diff
        minorSize = self._bar.region.size[flipOrientation(self._orientation)]
        self._bar.resize(orientedOrder(size, minorSize))
        diff = interval - self._bar.getValue()
        if diff < 0:
            self._bar.move(orientedOrder(diff, 0))
    def resize(self, size):
        # try first
        minorSize = self.region.size[flipOrientation(self._orientation)]
        minSize = self._bar.presentation.minSize()
        minBarSize = minSize[self._orientation]
        minMinorSize = minSize[flipOrientation(self._orientation)]
        if minorSize < minMinorSize:
            minorSize = minMinorSize
        total = self.region.size[self._orientation]
        barSize = total - self.interval
        if barSize < minBarSize:
            total += minBarSize - barSize
            barSize = minBarSize
        ElementContainer.resize(self, orientedOrder(total, minorSize))
        self._bar.resize(orientedOrder(barSize, minorSize))
#    def onSize(self, size):
#        ElementContainer.onSize(self, size)
#        self._bar.resize()
