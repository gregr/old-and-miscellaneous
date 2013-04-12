from time import clock
import sys
import os

cmd = " ".join(sys.argv[1:])
start = clock()
os.system(cmd)
end = clock()

print
print "seconds elapsed:", end-start
