# Gregory Rosenblatt
# 3/1/06

"""Sudoku Solver Generator
	
Generates SWI-Prolog programs that will solve the provided puzzles.
Puzzles should be written in a text file with the example format:

070030080
200090005
000807000
006000200
780000041
003000900
000203000
100070002
040010090

SWI-Prolog can be found at: http://www.swi-prolog.org

Usage: python makesolver.py [puzzle1 puzzle2 ...]
	
Example Main options
	-h                     show this help message
"""

import sys
import getopt
import math

def LoadPuzzle(inputFile):
	puzzle = {}
	for j, line in enumerate(inputFile):
		for i, ch in enumerate(line.strip()):
			try:
				val = int(ch)
			except ValueError:
				val = ord(ch.upper())-ord('A')+10
			if val != 0:
				puzzle[(i,j)] = val
	if i != j or (j % int(math.sqrt(j)) != 0):
		raise RuntimeError("%s does not describe a valid puzzle.")
	return [puzzle, j+1]

def MakeVarArrayString(vars):
	s = "\n".join("".join(v+', ' for v in row) for row in vars)
	return s[:len(s)-2]

def MakePuzzleAssignString(puzzle, vars):
	return "\n".join("%s = %d," % (vars[index[1]][index[0]], val)
									for index, val in puzzle.iteritems())

def MakeDiffString(vars):
	sub = "".join(v+', ' for v in vars)
	sub = sub[:len(sub)-2]
	return "all_different([%s])," % sub

def MakeWriteString(vars):
	size = len(vars)
	boxSize = int(math.sqrt(size))
	s = ""
	for x in xrange(boxSize):
		for i in xrange(x*boxSize, (x+1)*boxSize):
			s += "write(%s),write(' ')," % vars[i]
		s += "write(' '),"
	return s + "\nwrite('\\n'),\n"

def MakeRowDiffString(vars):
	return "\n".join(MakeDiffString(row) for row in vars)

def MakeColumnDiffString(vars):
	return "\n".join(MakeDiffString(col) for col in zip(*vars))

def MakeBoxDiffString(vars):
	size = len(vars)
	boxSize = int(math.sqrt(size))
	boxes = []
	for y in xrange(boxSize):
		for x in xrange(boxSize):
			box = []
			for j in xrange(y*boxSize, (y+1)*boxSize):
				for i in xrange(x*boxSize, (x+1)*boxSize):
					box.append(vars[j][i])
			boxes.append(box)
	return "\n".join(MakeDiffString(b) for b in boxes)

def MakeAllWriteStrings(vars):
	size = len(vars)
	boxSize = int(math.sqrt(size))
	s = ""
	for y in xrange(boxSize):
		for j in xrange(y*boxSize, (y+1)*boxSize):
			s += MakeWriteString(vars[j])
		s += "write('\\n'),\n"
	return s[:len(s)-2]+'.'

def WritePuzzleSolver(fileName):
	try:
		puzzle, size = LoadPuzzle(open(fileName))
	except IOError:
		print >>sys.stderr, "Could not open file:", fileName
		return
	except RuntimeError, e:
		print >>sys.stderr, str(e) % fileName
		return
	labels = [chr(i) for i in xrange(ord('A'), ord('A')+size)]
	vars = [[l+str(i) for l in labels] for i in xrange(1, size+1)]
	code = ((":- use_module(library('clp/bounds')).\n\n:-\nVars = [\n%s],\n\n"
			+"Vars in %d..%d,\n\n%s\n\n%s\n\n%s\n\n%s\n\nlabel(Vars),\n\n%s")
			% (MakeVarArrayString(vars), 1, size
				,MakePuzzleAssignString(puzzle, vars)
				,MakeRowDiffString(vars)
				,MakeColumnDiffString(vars)
				,MakeBoxDiffString(vars)
				,MakeAllWriteStrings(vars)))
	dot = fileName.find('.')
	if dot < 0:
		dot = len(fileName)
	prologFileName = fileName[:dot]+".pl"
	print >>open(prologFileName, 'w'), code

class Usage(Exception):
	def __init__(self, msg):
		self.msg = msg

def main(argv=None):
	if argv is None:
		argv = sys.argv
	try:
		# parse command line options
		try:
			opts, args = getopt.getopt(argv[1:], "h", ["help"])
		except getopt.GetoptError, msg:
			raise Usage(msg)
		for o, a in opts:
			if o in ("-h", "--help"):
				print __doc__
				return 0
		# each argument should be a puzzle filename
		for arg in args:
			WritePuzzleSolver(arg)
	except Usage, err:
		print >>sys.stderr, err.msg
		print >>sys.stderr, "for help use --help"
		return 2

if __name__ == '__main__':
	sys.exit(main())
