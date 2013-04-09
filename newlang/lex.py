import re

tokenTypes = ["eol", "whitespace", "syntax", "operator", "ident", "literal"]

# a literal should never be directly followed by another literal
def literalBoundary(s):
    return s != '"' and s != "'" and not s.isalpha()

def intBoundary(s):
    return s[0] != '.' and literalBoundary(s)

# still need special cases for things like: 4-.3 which will
# consider -. to be an operator... could simply redefine float literals
reWhitespace = re.compile(r"\s+"), None, "whitespace", len
reSyntax = re.compile(r"[()\[\]{}]"), None, "syntax", str
reOperator = (re.compile(r"[`~!@#$%^&*\-=+\\|:,.<>/?]+"), None,
            "operator", str)
reIdent = re.compile(r"[a-zA-Z_]\w*"), literalBoundary, "ident", str
reFloat = (re.compile(r"-?(\d*\.?\d+)([eE][-+]?\d+)?"), literalBoundary,
           "literal", float)
reInt = re.compile(r"-?(0x)?\d+"), intBoundary, "literal", int
reChar = re.compile(r"'(\\.|[^\\'])'"), literalBoundary, "literal", eval
reString = re.compile(r'"(\\.|[^\\"])*"'), literalBoundary, "literal", eval

commentChar = ';'

regexpsNonLiterals = (reSyntax, reOperator, reIdent)
regexpsLiterals = (reInt, reFloat, reChar, reString)
# minus could be an number's sign when following whitespace
regexpsWhitespace = (reWhitespace,)
regexpsAfterSpace = regexpsLiterals + regexpsNonLiterals
# minus should be a binop if directly following a literal
regexpsAdjacent = regexpsNonLiterals + regexpsLiterals

def matchAgainst(regexps, s):
    for regexp, validBoundary, tokenType, cast in regexps:
        m = regexp.match(s)
        if m is not None:
            end = m.end()
            if end == len(s) or validBoundary is None or validBoundary(s[end]):
                return (tokenType, cast(s[:end])), s[end:]
    return None, s

class LexError(StandardError): pass

def markedPos(s, pos):
    return s + '\n' + pos*' ' + '^'

def lineTokens(s):
    origLine = s
    s = s.rstrip()
    while s:
        token, s = matchAgainst(regexpsWhitespace, s)
        if s[0] == commentChar:
            if token is not None:
                yield token
            break
        if token is None:
            regexps = regexpsAdjacent
        else:
            yield token
            regexps = regexpsAfterSpace
        token, s = matchAgainst(regexps, s)
        if token is None:
            raise LexError, (origLine, len(origLine)-len(s))
        yield token
    yield "eol", None

def logicalLines(stream):
    prevLine = ""
    for line in stream:
        if prevLine:
            line = prevLine+line
            prevLine = ""
        if line.endswith('\\'):
            prevLine = line[:-1]
            continue
        yield line

def streamTokens(stream):
    for line in logicalLines(stream):
        for token in lineTokens(line):
            yield token
    yield "eol", None

def _lineTest(s="(f abc 2-3 -4)"):
    for t in lineTokens(s):
        print t

def _test(s):
    for t in streamTokens(s):
        print t
