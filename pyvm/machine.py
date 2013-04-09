import sys

class Machine(object):
    ops = {}
    def __init__(self, instructions, memorySize=1024*64):
        self.instructions = instructions
        self.memory = range(memorySize)
        self.clear()
    def clear(self):
        self.registers = {}
        self.ip = 0
    def run(self):
        self.running = True
        while self.running:
            ip = self.ip
            self.ip += 1
            instr, args = self.instructions[ip]
            instr(self, *args)

def makeop(f):
    Machine.ops[f.func_name] = f
    return f

@makeop
def add(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] + m.registers[b]
@makeop
def sub(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] - m.registers[b]
@makeop
def mul(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] * m.registers[b]
@makeop
def div(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] // m.registers[b]
@makeop
def mod(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] % m.registers[b]

@makeop
def addi(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] + b
@makeop
def subi(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] - b
@makeop
def muli(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] * b
@makeop
def divi(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] // b
@makeop
def modi(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] % b

@makeop
def shl(m, tgt, v, shift):
    m.registers[tgt] = m.registers[v] << m.registers[shift]
@makeop
def shr(m, tgt, v, shift):
    m.registers[tgt] = m.registers[v] >> m.registers[shift]
@makeop
def shli(m, tgt, v, shift):
    m.registers[tgt] = m.registers[v] << shift
@makeop
def shri(m, tgt, v, shift):
    m.registers[tgt] = m.registers[v] >> shift

@makeop
def load(m, tgt, addr, offset):
    m.registers[tgt] = m.memory[m.registers[addr]+offset]
@makeop
def loadi(m, tgt, const):
    m.registers[tgt] = const
@makeop
def store(m, src, addr, offset):
    m.memory[m.registers[addr]+offset] = m.registers[src]
@makeop
def swap(m, src, addr, offset):
    addr = m.registers[addr]+offset
    tmp = m.registers[src]
    m.registers[src] = m.memory[addr]
    m.memory[addr] = tmp

@makeop
def eq(m, tgt, a, b):
    m.registers[tgt] = int(m.registers[a] == m.registers[b])
@makeop
def gte(m, tgt, a, b):
    m.registers[tgt] = int(m.registers[a] >= m.registers[b])
@makeop
def gt(m, tgt, a, b):
    m.registers[tgt] = int(m.registers[a] > m.registers[b])

@makeop
def land(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] & m.registers[b]
@makeop
def lor(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] | m.registers[b]
@makeop
def lnor(m, tgt, a, b):
    m.registers[tgt] = ~(m.registers[a] | m.registers[b])
@makeop
def lxor(m, tgt, a, b):
    m.registers[tgt] = m.registers[a] ^ m.registers[b]

@makeop
def jez(m, ra, test, addr):
    if not m.registers[test]:
        jump(m, ra, addr)
@makeop
def jezi(m, ra, test, addr):
    if not m.registers[test]:
        jumpi(m, ra, addr)
@makeop
def jnz(m, ra, test, addr):
    if m.registers[test]:
        jump(m, ra, addr)
@makeop
def jnzi(m, ra, test, addr):
    if m.registers[test]:
        jumpi(m, ra, addr)
@makeop
def jump(m, ra, addr):
    m.registers[ra] = m.ip
    m.ip = m.registers[addr]
@makeop
def jumpi(m, ra, addr):
    m.registers[ra] = m.ip
    m.ip = addr

@makeop
def cin(m, tgt):
    m.registers[tgt] = sys.stdin.read(1)
@makeop
def cout(m, src):
    sys.stdout.write(str(m.registers[src]))
@makeop
def couti(m, val):
    sys.stdout.write(str(val))

@makeop
def nop(m):
    pass

@makeop
def halt(m):
    m.running = False

def _test():
    prog = (
        (loadi, ("n", 7)),
        (loadi, ("end", 10)),
        (loadi, ("loop", 5)),
        (loadi, ("one", 1)),
        (loadi, ("val", 1)),
        (gt, ("test", "n", "one")),
        (jez, ("ra", "test", "end")),
        (mul, ("val", "val", "n")),
        (subi, ("n", "n", 1)),
        (jump, ("ra", "loop",)),
        (cout, ("val",)),
        (loadi, ("nl", '\n')),
        (cout, ("nl",)),
        (halt,())
        )
    m = Machine(prog)
    m.run()
