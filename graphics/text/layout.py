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

# TODO: this could all be smarter to reduce work by only re-wrapping modified
# lines of a text block
# also would need to map wrapped x,y coords back to original text position
# for dealing with edit controls

def wordWrapped(shouldWrap, tabSize, text):
    """Create a list of the word-wrapped lines of the given text.

    Word-wrapping is determined by the given predicate on strings.
    This is a greedy algorithm like those used in word processors.
    """
    pending = ""
    for line in text.splitlines(): # it would be nice to have an isplitlines()
        for result, pending in _wrappedLine(shouldWrap, tabSize, line, pending):
            if result is not None:
                yield result
        if result is None:
            yield pending
            pending = ""

def _wrappedLine(shouldWrap, tabSize, line, pending):
    candidate = pending
    for tabbed in line.split("\t"): # it would be nice to have an isplit() too
        pending = candidate
        for result, pending in _wrappedTabbed(shouldWrap, tabbed, pending):
            if result is not None:
                yield result, pending
        candidate = pending + (" ")*(tabSize - (len(pending) % tabSize))
    yield None, pending

def _wrappedTabbed(shouldWrap, tabbed, pending):
    candidate = pending
    for word in tabbed.split(" "):
        pending = candidate
        for result, pending in _wrappedWord(shouldWrap, word, pending):
            if result is not None:
                yield result, pending
        candidate = pending + " "
    yield None, pending

def _wrappedWord(shouldWrap, word, pending):
    candidate = pending + word
    while shouldWrap(candidate):
        if pending:
            yield pending, word
            candidate = word
            pending = ""
        else:
            for result, pending in _wrappedSubWord(shouldWrap, candidate):
                yield result, pending
            break
    else:
        pending = candidate
        yield None, pending

def _wrappedSubWord(shouldWrap, subWord):
    pending = subWord[0]
    for i in xrange(1, len(subWord)):
        while shouldWrap(pending):
            if len(pending) == 1: # minimum of 1 character must be yielded
                yield pending, pending
                pending = ""
            else:
                yield pending[:-1], pending
                pending = subWord[i-1]
        pending += subWord[i]
    yield None, pending

def _test():
    s = "\thello there world... what\n have you been \n up to lately?"
    def printLines(lines):
        print "lines:"
        for line in lines:
            print repr(line)
        print
    def wrap(width):
        def shouldWrap(s):
            return len(s)*4 > width
        return shouldWrap
    printLines(wordWrapped(wrap(70), 4, s))
    printLines(wordWrapped(wrap(50), 4, s))
    printLines(wordWrapped(wrap(30), 4, s))
    printLines(wordWrapped(wrap(20), 4, s))
    printLines(wordWrapped(wrap(12), 4, s))
    printLines(wordWrapped(wrap(3), 4, s))

if __name__ == "__main__":
    _test()
