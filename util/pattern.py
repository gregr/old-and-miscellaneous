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

"""Useful programming patterns"""

from weakref import WeakValueDictionary


def isSingleton(obj, instances=WeakValueDictionary()):
    """Assert that the class of obj has not already been instantiated.

    Simply assert that this is true at the beginning of __init__.
    This should satisfy many cases of the singleton pattern.
    """
    if obj.__class__ in instances:
        return False
    instances[obj.__class__] = obj
    return True

def memoized(func):
    """Cache the results of the given function to optimize later calls.

    The first time the function is called with a set of arguments, the
    result will be cached.  Subsequent calls with the same arguments
    will return the cached value.
    """
    def memoizedFunc(*args):
        if memoizedFunc.results.has_key(args):
            return memoizedFunc.results[args]
        memoizedFunc.results[args] = result = func(*args)
        return result
    memoizedFunc.results = {}
    return memoizedFunc

class Pooled(object):
    """A pooled-instance class to be derived from

    Instances of derived classes are stored in a class-specific pool
    keyed by their initialization argument signature. If an instance has
    already been created with a particular set of arguments, invoking
    the constructor with the same arguments will produce a reference to
    the already-existing instance.  The constructor will only create new
    instances when an appropriate one is not found in the pool.

    **kwargs parameter is not supported in __init__.
    The name "__pool__" is reserved and must not be redefined.
    """
    class __metaclass__(type):
        def __init__(cls, name, bases, namespace):
            type.__init__(cls, name, bases, namespace)
            cls.__pool__ = WeakValueDictionary()
        def __call__(cls, *args):
            if cls.__pool__.has_key(args):
                return cls.__pool__[args]
            obj = cls.__new__(cls, *args)
            obj.__init__(*args)
            cls.__pool__[args] = obj
            return obj

class PooledFactory(object):
    """A factory which pools the objects produced by an independent function

    Instances of this class should be used when it is preferrable to
    use an existing function as a constructor.

    Produced instances are stored in a pool keyed by their construction
    argument signature.  If an instance has already been created with a
    particular set of arguments, invoking the factory with the same
    arguments will produce a reference to the already-existing instance.
    The constructor will only create new instances when an appropriate
    one is not found in the pool.
    """
    def __init__(self, construct):
        """Create a factory to produce instances using a given constructor."""
        self.pool = WeakValueDictionary()
        self.construct = construct

    def __call__(self, *args):
        """Produce an instance constructed from the given arguments.

        Retrieve an appropriate instance from the pool if it exists,
        Otherwise, construct a new instance and store it.
        """
        if self.pool.has_key(args):
            return self.pool[args]
        self.pool[args] = obj = self.construct(*args)
        return obj
