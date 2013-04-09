from interpreter import evalStream, evalFile, go
from data import null
from pretty import pretty
from StringIO import StringIO
import sys

def interact(prompt="eval> "):
    prompt2 = ((len(prompt)-1)*'.')+' '
    buffer = []
    try:
        pmt = prompt
        while True:
            line = raw_input(pmt)
            pmt = prompt2
            buffer.append(line+'\n')
            if not line:
                val = null
                try:
                    for val in evalStream(StringIO("".join(buffer))):
                        pass
                    if val is not null:
                        print pretty(val)
                except Exception, e:
                    print "ERROR"
                    print e
                buffer = []
                pmt = prompt
    except EOFError:
        pass
    print

go(evalFile("prelude.scm"))

if len(sys.argv) > 1:
    go(evalFile(sys.argv[1]))

interact()
