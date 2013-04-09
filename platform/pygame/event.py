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
from pygame.locals import *
import logging


class _pygameInstance(object):
    """Initializes/shuts down pygame when constructed/destroyed

    Only one instance can be active at a time.
    """

    _initialized = False

    def __init__(self):
        """Initialize pygame."""
        assert not self._initialized
        pygame.init()
        self._initialized = True
        logging.info("Initialized pygame")

    def __del__(self):
        """Shut down pygame."""
        pygame.quit()
        self._initialized = False
        logging.info("Shut down pygame")


class Dispatcher(object):
    """An event dispatcher for use with pygame

    Dispatches pygame events to their appropriate handlers where specified.
    Only one instance should exist at any given time.
    """

    def __init__(self):
        """Prepare the event loop."""
        self._pygameInstance = _pygameInstance()
        self.resetHandlers()

    def resetHandlers(self):
        """Remove all event handlers and restore defaults."""
        def stop(e):
            raise StopIteration
        self._eventHandlers = {QUIT: stop}
        pygame.event.set_allowed(None) # this should block all event types
        self.addHandlers({}) # then add them back in selectively

    def addHandlers(self, handlers):
        """Set additional event handlers.

        Only event types with handlers will be processed by the loop.
        If a handler for QUIT is not provided, a default will be used.
        """
        self._eventHandlers.update(handlers)
        keys = self._eventHandlers.keys()
        pygame.event.set_allowed(keys)

    def start(self, update=lambda:None):
        """Start the event loop.

        The given update procedure will be called once per iteration.
        """
        for none in iter(self):
            update()

    def __iter__(self):
        """Create an event loop iterator.

        Iteration ends after receiving a QUIT event by default.
        """
        return _iterEvents(self._eventHandlers)


def _iterEvents(handlers):
    # this will at least save a couple extra object lookups
    # however much that matters...
    _getEvents = pygame.event.get
    while True:
        for e in _getEvents():
            handlers[e.type](e)
        yield
