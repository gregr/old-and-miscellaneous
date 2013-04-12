nCalls = 0

def fib(n):
    global nCalls
    nCalls += 1
    if n < 2:
        return n
    return fib(n-1) + fib(n-2)

for n in xrange(36):
    print fib(n)
print
print nCalls
