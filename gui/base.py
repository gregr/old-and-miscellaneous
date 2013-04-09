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

from output import Presentation, ThemeMap
from input import Cursor, Caret
from uriel.util.tuplevector import add as addTuples, sub as subTuples
from weakref import proxy


class Region(object):
    """A two-dimensional region within a user interface."""

    def __init__(self, pos, size):
        """Create a region with the given metrics."""
        self.pos = pos
        self.size = size

    def move(self, rel):
        """Move the region."""
        self.pos = addTuples(self.pos, rel)

#    def unmove(self, rel):
#        """Move the region in the opposite direction."""
#        self.pos = subTuples(self.pos, rel)

    def resize(self, size):
        """Change the size of the region."""
        self.size = size

    def containsPos(self, pos):
        """Determine if a position lies within this region."""
        px, py = pos
        x, y = self.pos
        w, h = self.size
        x = px - x
        y = py - y
        return (x >= 0) and (x < w) and (y >= 0) and (y < h)

    def containsRegion(self, region):
        """Determine if a region lies entirely within this region."""
        return (self.containsPos(region.pos) and
                self.containsPos(subTuples(region.extent, (1, 1))))

    def intersectsRegion(self, region):
        """Determine if any part of a region lies within this region."""
        w, h = self.intersection(region).size
        return (w > 0) and (h > 0)

    def intersection(self, region):
        """Return the intersection of this region with another region."""
        x, y = self.pos
        u, v = self.extent
        xx, yy = region.pos
        uu, vv = region.extent
        if x < xx: x = xx
        if y < yy: y = yy
        if u > uu: u = uu
        if v > vv: v = vv
        return Region((x, y), (u-x, v-y))

    def _getExtent(self):
        return addTuples(self.pos, self.size)

    def _setExtent(self, extent):
        self.size = subTuples(extent, self.pos)

    extent = property(_getExtent, _setExtent) # bottom right point


class Element(object):
    """The building block of a user interface"""

    themes = ThemeMap(Presentation)
    
    def __init__(self, parent, pos=(0,0), size=(0,0), theme=None):
        """Create an element with the given properties."""
        self.region = Region(pos, size)
        if parent is not None:
            self.setParent(parent)
        else:
            self.parent = None
        self._active = False
        self._visible = True
        self._shown = True
        self._setTheme(theme)

    def setTheme(self, theme=None, force=False):
        """Set the theme of this element.

        The theme will be set if the element only has a default theme or if the
        force flag is set.
        """
        if force or self.theme is None:
            self.forceSetTheme(theme, force)

    def forceSetTheme(self, theme=None, force=False):
        """Set the theme of this element.

        The theme will always be set.
        """
        self._setTheme(theme)

    def resize(self, size):
        """Change the size of this element."""
        self.region.resize(size)
        self._updateVisibleRegion()
        self.onSize(size)

    def move(self, rel):
        """Move this element."""
        self.region.move(rel)
        self._updateVisibleRegion()
        self.onMove(rel)

    def setParent(self, parent):
        """Set this element's parent.

        This should be used when this element currently has no parent, as the
        element will also be moved so that its current absolute position
        becomes its relative position within the new parent.
        """
        self._attach(parent)
        self.region.move(parent.region.pos)
        self._updateVisibleRegion()

    def changeParent(self, parent):
        """Set this element's parent.
        
        This should be used when this element already has a parent.
        """
        self._detach()
        self._attach(parent)
        self._updateVisibleRegion()

    def show(self):
        """Make this element visible and pickable."""
        self._shown = True
        if self.parent.visible:
            self.visible = True

    def hide(self):
        """Make this element invisible and unpickable."""
        self._shown = False
        self.visible = False

    def moveToTop(self):
        """Move this element to the top of the hierarchy.

        This also recursively moves its ancestors to the top.
        """
        p = self.parent
        p._moveChildToTop(self)
        self.active = True
        p.moveToTop()

    def getAtPos(self, pos):
        """Return this element."""
        return self

    def __iter__(self):
        yield self

    def _attach(self, parent):
        self.parent = proxy(parent)
        self.parent.add(self)

    def _detach(self):
        self.parent.remove(self)
        self.parent = None

    def _moveWithParent(self, rel):
        self.region.move(rel)
        self.visibleRegion.move(rel)
        self.onMove(rel)

    def _setTheme(self, theme=None):
        if (theme is None) and (self.parent is not None):
            theme = self.parent.presentation.theme
        self.presentation = self.themes[theme](proxy(self))
        self.theme = theme

    def _updateVisibleRegion(self):
        self.visibleRegion = self.parent.visibleRegion.intersection(
            self.region)

    def _isActive(self):
        return self._active

    def _setActive(self, value):
        assert isinstance(value, bool)
        if self._active != value:
            self._active = value
            if value:
                self.onActivate()
            else:
                self.onDeactivate()

    def _isVisible(self):
        return self._visible

    def _setVisible(self, value):
        assert isinstance(value, bool)
        if self._visible != value:
            self._visible = value
            if value:
                if self._shown:
                    self.onVisible()
            else:
                self.onInvisible()

    active = property(_isActive, _setActive)
    visible = property(_isVisible, _setVisible)

    def onMove(self, rel):
        self.presentation.onMove()
    def onSize(self, size):
        self.presentation.onSize()
    def onActivate(self):
        self.presentation.onActivate()
    def onDeactivate(self):
        self.presentation.onDeactivate()
    def onVisible(self):
        self.presentation.onVisible()
    def onInvisible(self):
        self.presentation.onInvisible()
    def onCursorMove(self, cursor):
        self.presentation.onCursorMove()
    def onCursorGain(self, cursor):
        self.presentation.onCursorGain()
    def onCursorLose(self, cursor):
        self.presentation.onCursorLose()
    def onButtonDown(self, cursor, button):
        self.presentation.onButtonDown()
    def onButtonUp(self, cursor, button):
        self.presentation.onButtonUp()
    def OnCaretGain(self, caret):
        self.presentation.onCaretGain()
    def OnCaretLose(self, caret):
        self.presentation.onCaretLose()
    def OnKeyDown(self, caret, key):
        self.presentation.onKeyDown()
    def OnKeyUp(self, caret, key):
        self.presentation.onKeyUp()


class ElementContainer(Element):
    """An element that can contain other elements."""
    def __init__(self, *args, **kwargs):
        Element.__init__(self, *args, **kwargs)
        self.children = [] # top children are stored at the end of the list

    def forceSetTheme(self, theme=None, force=False):
        """Set the theme of this element and its children.

        The theme and force flag will be propogated.
        """
        Element.forceSetTheme(self, theme, force)
        for child in self.children:
            child.setTheme(theme, force)
        
    def add(self, child):
        """Add a child to this container."""
        self.children.append(child)
        self.presentation.onHeightChange()

    def remove(self, child):
        """Remove a child from this container."""
        self.children.remove(child)
        self.presentation.onHeightChange()

    def getAtPos(self, pos):
        """Get the top element at a position."""
        for child in reversed(self.children):
            if child.visible:
                if child.region.containsPos(pos):
                    return child.getAtPos(pos)
        return self

    def __iter__(self):
        yield self
        for child in self.children:
            for element in child:
                yield element

    def _updateVisibleRegion(self):
        Element._updateVisibleRegion(self)
        for child in self.children:
            child._updateVisibleRegion()

    def _topChild(self):
        return self.children[-1]

    def _moveChildToTop(self, child):
        top = self._topChild()
        if top is not child:
            self.remove(child)
            self.add(child)
            top.active = False

    def onMove(self, rel):
        """Handle movement and signal the event to all children."""
        Element.onMove(self, rel)
        for child in self.children:
            child._moveWithParent(rel)

    def onVisible(self):
        """Handle becoming visible and signal the event to all children."""
        Element.onVisible(self)
        for child in self.children:
            child.visible = True

    def onInvisible(self):
        """Handle becoming invisible and signal the event to all children."""
        Element.onInvisible(self)
        for child in self.children:
            child.visible = False

    def onButtonDown(self, cursor, button):
        """Handle a cursor button-down event and take the active state."""
        Element.onButtonDown(self, cursor, button)
        self._topChild().active = False # steal active state from child


class Root(ElementContainer):
    def __init__(self, size):
        """Create a root element."""
        ElementContainer.__init__(self, None, (0,0), size)
        self.parent = None
        self.visibleRegion = self.region
        self.cursor = Cursor(self)
        self.caret = Caret(self)

    def show(self):
        """Make this root visible and pickable."""
        self._shown = True
        self.visible = True

    def moveToTop(self):
        pass

    def _updateVisibleRegion(self):
        for child in self.children:
            child._updateVisibleRegion()
