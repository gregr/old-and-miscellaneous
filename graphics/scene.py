# Copyright (C) 2008 Gregory L. Rosenblatt
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

"""Hierarchical scenes"""

from collections import deque

def preOrder(root): # depth-first: possibility for UI if top child is last
    yield root
    for child in root.children:
        for node in postOrder(child):
            yield node

def postOrder(root): # also could be good for UI if top child is first?
    for child in root.children:
        for node in postOrder(child):
            yield node
    yield root

def levelOrder(root): # breadth-first: good for layered 2d map with spillover
    q = deque()
    q.append(root)
    while q:
        node = q.popleft()
        yield node
        q.extend(node.children)

class Node(object):
    def __init__(self):
        self.children = []
