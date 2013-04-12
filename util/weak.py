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

"""An extended collection of weak-referencing types"""

from weakref import ref, proxy
from functools import partial


def weakMethod(method, *args, **kwargs):
    """Create a bound method which only weakly references the instance.

    Optionally, additional arguments may be supplied to create a
    partial application of the method.
    """
    func = method.im_func
    selfProxy = proxy(method.im_self)
    return partial(func, selfProxy, *args, **kwargs)


class WeakSet(object):
    """A set which references its contents weakly

    Objects are identified and hashed by their resulting weakref.

    A contained object will be discarded when there is no longer a
    strong reference to it.
    """

    def __init__(self, sequence=[]):
        """Create a WeakSet with an internal conventional set for storage."""
        self.set = set(ref(obj) for obj in sequence)

    def __del__(self):
        """Ensure proper cleanup of the internal set."""
        # If there are objs in the set, then they have references to self.set
        # via the bound method self.set.remove, thus the set must be cleared
        # manually.  There should be no circular ref problem as long as
        # external objs only refer to the WeakSet, and objs contained only
        # weakly-reference WeakSet (if at all)
        self.set.clear()

    def __iter__(self):
        """Return an iterator that produces the objects present in this set."""
        for robj in self.set:
            yield robj()

    def __contains__(self, obj):
        """Determine if the given object is a member of this set."""
        return ref(obj) in self.set

    def __len__(self):
        """Return the cardinality of this set."""
        return len(self.set)

    def __repr__(self):
        """Return a string representing the contents of this set."""
        return "WeakSet(%r)" % self.set

    def add(self, obj):
        """Add a weakly-referenced object to the set."""
        self.set.add(ref(obj, self.set.remove))

    def remove(self, obj):
        """Remove an object from the set.

        It is an error to remove an object that is not present.
        See discard.
        """
        self.set.remove(ref(obj))

    def discard(self, obj):
        """Remove an object if it is present in the set.

        It is not an error to discard an object that is not present.
        """
        self.set.discard(ref(obj))
