'''Count Lines
        
Counts the number of lines in the given source files.
Usage: python countlines.py [sourcefile1.py sourcefile2.py ...]
        
Example Main options
        -h                     show this help message
'''
def fileLines(fn):
    nlines = 0
    for line in open(fn): nlines += 1
    print '%s: %i' % (fn, nlines)
    return nlines

def filesLines(fns):
    totalLines = 0
    for fn in fns: totalLines += fileLines(fn)
    if totalLines > 0:
        print
        print 'dir total:', totalLines
        print
    return totalLines

import os
import os.path

def dirFiles(d):
    files = []
    dir, subs, filenames = d
    for fn in filenames:
        name, ext = os.path.splitext(fn)
        if ext in ['.ax', '.py', '.pyx', '.pxi', '.h', '.cpp']:
            files.append(os.path.join(dir, fn))
    return files

def recDirFilesLines(root=''):
    total = 0
    for d in os.walk(root):
        print 'dir: ', d[0]
        total += filesLines(dirFiles(d))
    return total

import sys
import getopt

class Usage(Exception):
    def __init__(self, msg): self.msg = msg

def main(argv=None):
    if argv is None: argv = sys.argv
    try:
        try: opts, args = getopt.getopt(argv[1:], 'h', ['help'])
        except getopt.GetoptError, msg:
            raise Usage(msg)
        for o, a in opts:
            if o in ('-h', '--help'):
                print __doc__
                return 0
        grandTotal = 0
        for root in args:
            rootTotal = recDirFilesLines(root)
            grandTotal += rootTotal
            if rootTotal > 0:
                print
                print "root total:", rootTotal
                print
        if grandTotal > 0:
            print
            print "grand total:", grandTotal
            print
    except Usage, err:
        print >>sys.stderr, err.msg
        print >>sys.stderr, 'for help use --help'
        return 2

if __name__ == '__main__': sys.exit(main())
