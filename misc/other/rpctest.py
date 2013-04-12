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

from jsonrpc.json import dumps, loads


data = dumps({"method": "runTest", 'params': (1, "yes", True), 'id':'jsonrpc'})

print repr(data)

objs = loads(data)

print repr(objs)

#class ServiceProxy(object):
#    def __init__(self, addr, name):
#        pass
#    def __call__(self, *args):
