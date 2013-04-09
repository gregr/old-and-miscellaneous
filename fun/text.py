from bisect import bisect

# traits = font, color, bgcolor
# offset = int # if 0?, starts a new line, otherwise it continues the prev line
# line = [char]
# wrappedLine = [(index, line)]
# group = (traits, offset, [line], [wrappedLine])
# buffer = [group]

class Group(object):
    def __init__(self): # should always maintain both reprs
        self.lines = []
        self.wrappedLines = []
    def _wrappedCoordToPos(self, x, y):
        for lineNum, lines in enumerate(self.wrappedLines):
            height = len(lines)
            if height > y:
                index, line = lines[y]
                x += index
                break
            y -= height
        return x, lineNum
    def _wrappedPosToCoordDelta(self, x, y, dx, dy):
        wrapped = self.wrappedLines[y]
        dy = bisect(wrapped, (x, None)) - 1

class Buffer(object):
    pass
