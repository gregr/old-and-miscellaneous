from assembly import assemble, parseProg
from machine import Machine

def makeMachine(fileName):
    return Machine(list(assemble(parseProg(open(fileName)), Machine.ops)))

def run(fileName):
    m = makeMachine(fileName)
    m.run()

def prof(fileName):
    import time
    start = time.clock()
    run(fileName)
    print "time elapsed:", time.clock() - start

if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print "usage: python runasm.py FILENAME"
    else:
        prof(sys.argv[1])
