import re

reLabel = re.compile(r"[a-zA-Z_]\w*:")
reSymbol = re.compile(r"[$*+&%#@!~^]")

reIdent = re.compile(r"[a-zA-Z_]\w*")
reInt = re.compile(r"-?(0x)?\d+\b")
reFloat = re.compile(r"-?(\d*\.?\d+)([eE]\d+)?")
reChar = re.compile(r"'(\\.|[^\\'])'")
reString = re.compile(r'"(\\.|[^\\"])*"')

commentChar = ';'

def isBoundary(c):
    return c.isspace() or c == commentChar

def isComment(s):
    return s[0] == commentChar

def skipSpacesAndComments(s):
    s = s.lstrip()
    if s and isComment(s):
        return ""
    return s

def matchAgainst(regexps, s, first=False):
    s = skipSpacesAndComments(s)
    for regexp, cast in regexps:
        m = regexp.match(s)
        if m is not None:
            end = m.end()
            if first or end == len(s) or isBoundary(s[end]):
                return cast(s[:end]), s[end:]
    return None, s

def ident(s):
    return s

def label(s):
    return s[:-1]

def char(s):
    return eval(s)
#    return ord(str(s))

def string(s):
    return eval(s)

regexpsInitial = (
    (reLabel, label),
    )

regexpsOperator = (
    (reIdent, ident),
    )

regexpsSymbol = (
    (reSymbol, str),
    )

regexpsOperands = (
    (reIdent, ident),
    (reInt, int),
    (reFloat, float),
    (reChar, char),
    (reString, string)
    )

def parseLine(line):
    s = line
    label, s = matchAgainst(regexpsInitial, s)
    if label is None and not s:
        return None # blank line
    op, s = matchAgainst(regexpsOperator, s)
    if op is None and not s:
        instr = None # label-only line
    else:
        operands = []
        instr = (op, operands)
        while s:
            if op is None:
                raise RuntimeError, ("error while parsing: %d, %s, %s"
                                     % (len(line)-len(s), line, instr))
            sym, s = matchAgainst(regexpsSymbol, s, True)
#            print sym, s
            op, s = matchAgainst(regexpsOperands, s)
#            print op, s
            if op is not None:
                operands.append((sym, op))
    return label, instr

def parseProg(stream):
    prog = []
    for line in stream:
        result = parseLine(line)
        if result is not None:
            prog.append(result)
    return prog

def scanLabels(prog):
    labels = {}
    current = 0
    for label, instr in prog:
        if label is not None:
            line = labels.get(label)
            if line is not None:
                raise RuntimeError, ("duplicate label found on lines %s and %s"
                                     % (line, current))
            labels[label] = current
        if instr is not None:
            current += 1
    return labels

def makeReg(regName):
    return regName

regSym = "%"
labelSym = "$"
#constSym = ""

def assemble(prog, opMap):
    labels = scanLabels(prog)
    def labelToAddr(label):
        return labels[label]
    handleOperand = {None: lambda a: a,
                     regSym: makeReg,
                     labelSym: labelToAddr}
    def processOperand((sym, val)):
        return handleOperand[sym](val)
    for _, instr in prog:
        if instr is not None:
            op, operands = instr
            yield opMap[op], map(processOperand, operands)
