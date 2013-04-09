import re

tokenTypes = ('whitespace', 'comment', 'meta', 'indentation',
              'syntax', 'operator', 'ident', 'literal')
ignoredTokens = ('whitespace', 'comment')

def makeIdent(s):
    return "\\".join(ss.replace('\\', '') for ss in str(s).split('\\\\'))
def makeIdentOp(s): return makeIdent(s[1:-1])

# todo: u32, i32, etc.
def makeInt(s): return ('int', int(s))
def makeFloat(s): return ('float', float(s))
def makeChar(s): return ('char', eval(s))
def makeString(s): return ('string', eval(s))

oper = '[`~!@$%^&*\\=+|;:,.<>/?-]+'
identPat = r'([a-zA-Z_]|(\\.))(\w|(\\.))*'
reWhitespace = re.compile(r'\s+'), 'whitespace', len
reCommentLine = re.compile(r'##.*'), 'comment', str
reSyntax = re.compile(r'#?[()\[\]{}]'), 'syntax', str
reMeta = re.compile(r'#(('+oper+')|(((\\.)|\w)+))'), 'meta', str
reOperator = re.compile(oper), 'operator', str
reIdent = re.compile(identPat), 'ident', makeIdent
reIdentOp = re.compile('`%s`'%identPat), 'operator', makeIdentOp
reFloat = re.compile(r'-?(\d+\.\d+)([eE][+-]?\d+)?'), 'literal', makeFloat
reInt = re.compile(r'-?(0x)?\d+'), 'literal', makeInt
reChar = re.compile(r"'((\\.)|[^\\'])'"), 'literal', makeChar
reString = re.compile(r'"((\\.)|[^\\"])*"'), 'literal', makeString

regexpsWhitespace = (reWhitespace,)
regexpsNonSeparating = (reIdent, reIdentOp, reFloat, reInt, reChar, reString)
regexpsSeparating = (reWhitespace, reCommentLine, reSyntax, reOperator, reMeta)
regexpsAny = regexpsNonSeparating+regexpsSeparating

def matchAgainst(regexps, s):
    for regexp in regexps:
        r, tokenType, cast = regexp
        m = r.match(s)
        if m is not None:
            end = m.end()
            return (tokenType, cast(s[:end])), s[end:], end, regexp
    return None, s, 0, None

class LexError(StandardError): pass

def markedPos(s, pos):
    return s + '\n' + pos*' ' + '^'

def lineTokens(s, lineNum):
    linePos = 0
    origLine = s
    s = s.rstrip()
    def attr(length): return (origLine, lineNum, linePos, length)
    if s:
        token, s, cs, _ = matchAgainst(regexpsWhitespace, s)
        if token is not None:
            indent = ('indentation', token[1]), attr(cs)
            linePos+=cs
        else: indent = ('indentation', 0), attr(0)
        regexps = regexpsAny
        firstToken = True
        while s:
            token, s, cs, regexp = matchAgainst(regexps, s)
            if token is None:
                if regexps is regexpsSeparating:
                    token, _, cs, _ = matchAgainst(regexpsAny, s)
                    if token is not None:
                        raise LexError, ('expected valid separator; found %s' %
                                         token[0], attr(cs))
                raise LexError, ('unknown token type', attr(0))
            if token[0] not in ignoredTokens:
                if firstToken: # sad, the least complicated way in python
                    yield indent
                    firstToken = False
                yield token, attr(cs)
            linePos+=cs
            if regexp in regexpsSeparating: regexps = regexpsAny
            else: regexps = regexpsSeparating

def logicalLines(stream):
    prevLine = ''
    lineNum = 0
    skippedLines = 0
    for line in stream:
        lineNum += 1
        if line.endswith('\\\n'):
            prevLine += line[:-2]
            skippedLines += 1
        else:
            yield prevLine+line, lineNum-skippedLines
            prevLine = ''
            skippedLines = 0
    if prevLine: yield prevLine, lineNum

def tokenize(stream):
    lineNum = 0
    for line, lineNum in logicalLines(stream):
        for token in lineTokens(line, lineNum): yield token
    yield ('indentation', -1), ('', lineNum+1, 0)

def _lineTest(s=r'(f abc 2-3 -4 \5\+def)'):
    for t in lineTokens(s, 1): print t

def _test(s):
    from StringIO import StringIO
    for t in tokenize(StringIO(s)): print t

#_test('hello world\n  4+ 3\n\n  5 - 6\n  \n## comments\n\n')
