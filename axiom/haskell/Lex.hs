module Lex where
import Util
import Numeric
import Data.Char
import Data.Maybe
import Data.List

-- todo: negative numeric literals... ~ as a unary prefix negation operator?

data Token = Bracket Name [Token] Attr
           | Ident Name Attr
           | Oper Name Attr
           | Lit LitVal Attr
           | Newline Int Attr
             deriving Show

data LitVal = IntVal Integer
            | FloatVal Double
            | CharVal Char
            | StringVal String
              deriving Show

type Name = String
type Attr = (Int, Int)

-- keep these details as declarative as possible for flexibility: don't specify
-- these constants in the implementation
appBracket = openApp
listBracket = openList
setOrDictBracket = "{"
hashAppBracket = "#("
hashListBracket = "#["
hashSetOrDictBracket = "#{"

bracketPairs = [
 (appBracket, closeApp),
 (listBracket, closeList),
 (setOrDictBracket, "}"),
 (hashAppBracket, ")"),
 (hashListBracket, "]"),
 (hashSetOrDictBracket, "}")]

nonHashBracket (b, bc) rest = if isPrefixOf "#" b then rest else b++bc++rest
hashBracket (b, _) rest = if isPrefixOf "#" b then tail b++rest else rest
hashBracketChars = foldr hashBracket "" bracketPairs
bracketChars = foldr nonHashBracket "" bracketPairs

newlineChars = "\n\r\v"
spaceChars = " \t"
tabWidth = 4
operChars = "`~!@$%^&*-+=|:;<>,.?/" -- something special for backtick instead?

firstRow = 1
firstColumn = 0
initAttr = (firstRow, firstColumn)

tokens s = tokens' s initAttr

tokens' [] _ = []
tokens' s (row, col) = token : tokens' s' attr
    where (token, s', attr) = makeToken stripped (row, col')
          (spaces, stripped) = spanSpaces s
          col' = col+length spaces

srcErr (row, col) msg = error $ (show row) ++ ':':(show col) ++ ':':' ':msg

tokenizers = [
 (isDigit, makeNumeric),
 (isAlpha, makeIdent),
 (isOper, makeOper),
 (isNewline, makeNewline),
 (isBracket, makeBracket)]

makeToken s@(c:cs) attr@(row, col) = makeToken' tokenizers
    where makeToken' [] = dispatchChar c cs (row, col+1)
          makeToken' ((pred, tok):rest) =
              if pred c then tok s attr else makeToken' rest

dispatchChar '\\' cs a = makeToken s' a'
    where (s', a') = escapeChar cs a
dispatchChar '\'' cs a = makeCharVal cs a
dispatchChar '"' cs a = makeStringVal cs a
dispatchChar '#' cs a = doMacro cs a
dispatchChar c cs a = srcErr a $ "invalid token: " ++ (c:[])

escapeChar [] a = srcErr a "unexpected end of stream while escaping character"
escapeChar ('\n':cs) (r, _) = (cs, (r+1, firstColumn))
escapeChar cs a = srcErr a "invalid escape sequence"

--todo: special hashed brackets
doMacro ('#':cs) (r, c) = makeToken s (r, (c+length comment))
    where (comment, s) = break isNewline cs
doMacro (b:cs) a = if isHashBracket b then makeBracketToken ('#':[b]) cs a
                     else srcErr a "unknown lex macro"
doMacro _ a = srcErr a "unknown lex macro"

spaceSize ' ' = 1
spaceSize '\t' = tabWidth
spanSpaces s = span isSpaceChar s

isSpaceChar c = c `elem` spaceChars
isNewline c = c `elem` newlineChars
isOper c = c `elem` operChars --isSymbol c || isPunctuation c
isBracket c = c `elem` bracketChars
isHashBracket c = c `elem` hashBracketChars

makeBracketToken b cs a@(r, c) = (Bracket b [] a, cs, (r, c+1))
makeBracket (c:cs) a = makeBracketToken [c] cs a

makeNewline (_:s) (r, _) = (Newline indent attr, s', attr)
    where (spaces, s') = span isSpaceChar s
          indent = sum $ map spaceSize spaces
          attr = (r+1, indent)

makeOper s a@(r, c) = (Oper oper a, s', (r, c+length oper))
    where (oper, s') = span isOper s

--todo: negative numbers?
--todo: binary, octal, hex
--makeNumeric (0:cs) = error ""
makeNumeric s a =
    case s' of
      [] -> intResult
      (c:s'') -> if c `elem` ".eE" then makeFloatVal digs s' a else intResult
    where (digs, s') = spanDigits s
          intResult = makeIntVal digs s' a

spanDigits s = span isDigit s

makeIntVal digs rest a@(r, c) =
    (Lit (IntVal $ read digs) a, rest, (r, c+length digs))

makeFloatVal digs rest a@(r, c) =
    case readFloat whole of
      [(flt, s'')] -> case s'' of
                        [] -> (Lit (FloatVal flt) a, s', (r, c+length whole))
                        _ -> err
      [] -> err
    where (rhs, s') = spanFloatRhs rest a
          whole = digs++rhs
          err = srcErr a $ "invalid float literal: " ++ whole

spanFloatRhs s a =
    case s of
      [] -> err
      '.':cs -> let (digs, s') = spanDigits cs
                    noExp = ('.':digs, s') in
                case s' of
                  c:cs -> if c `elem` "eE" then
                              let (exp, s') = spanExponent cs a in
                              ('.':digs ++ c:exp, s')
                          else noExp
                  [] -> noExp
      c:cs -> if c `elem` "eE" then let (exp, s') = spanExponent cs a in
                                    (c:exp, s')
              else err
    where err = srcErr a "invalid float literal"

spanExponent [] a = srcErr a "unexpected end of stream reading float exponent"
spanExponent s@(c:cs) _ =
    if c `elem` "+-" then let (digs, s') = spanDigits cs in (c:digs, s')
    else let (digs, s') = spanDigits s in (digs, s')

makeIdent s a@(r, c) = (Ident ident a, s', (r, c+length ident))
    where (ident, s') = span isAlphaNum s

makeChar [] a = srcErr a "unexpected end of stream while reading char"
makeChar ('\\':cs) a = makeEscapedChar cs a
makeChar (c:cs) _ = (c, cs, 1)

--todo: \xHEX, \oOCT and \DEC codes
makeEscapedChar [] a = srcErr a "unexpected end of stream while escaping char"
makeEscapedChar ('\\':cs) _ = ('\\', cs, 2)
makeEscapedChar ('\'':cs) _ = ('\'', cs, 2)
makeEscapedChar ('"':cs) _ = ('"', cs, 2)
makeEscapedChar ('t':cs) _ = ('\t', cs, 2)
makeEscapedChar ('n':cs) _ = ('\n', cs, 2)
makeEscapedChar ('r':cs) _ = ('\r', cs, 2)
makeEscapedChar ('v':cs) _ = ('\v', cs, 2)
makeEscapedChar (ch:cs) a = srcErr a $ "unknown escape char: " ++ show ch
-- case readLitChar s of
--                [] -> error "unexpected end of stream"
--                [(c, rest)] -> (c, rest)

makeCharVal s a@(row, col) =
    if x /= '\'' then srcErr a $ "expected end of char literal, found: " ++ [x]
    else (Lit (CharVal c) a, rest, (row, col+len+1))
        where (c, x:rest, len) = makeChar s a

makeString ('\"':s) _ = ([], s, 1)
makeString s a@(row, col) = (c:rest, s'', len+lenRest)
    where (c, s', len) = makeChar s a
          (rest, s'', lenRest) = makeString s' (row, col+len)

makeStringVal s a@(r, c) = (Lit (StringVal str) a, s', (r, c+len))
    where (str, s', len) = makeString s a

--------------------------------
-- additional utilities
--------------------------------
matchBracketItemErr items err b a = fromMaybe err (lookup b items)
matchBracketItem items b a = matchBracketItemErr items err b a
    where err = srcErr a $ "invalid opening bracket: " ++ b

matchBracketDflt b a = matchBracketItemErr bracketPairs b b a
matchBracket b a = matchBracketItem bracketPairs b a

getAttr (Bracket _ _ a) = a
getAttr (Ident _ a) = a
getAttr (Oper _ a) = a
getAttr (Lit _ a) = a
getAttr (Newline _ a) = a

showTokens sep ts = join sep $ map showToken ts
showToken (Bracket b ts a) = b++(showTokens " " ts)++(matchBracketDflt b a)
showToken (Ident n _) = n
showToken (Oper n _) = n
showToken (Lit v _) = showLit v
showToken (Newline indent _) = '\n':(take indent $ repeat ' ')

showLit (IntVal i) = show i
showLit (FloatVal f) = show f
showLit (CharVal c) = show c
showLit (StringVal s) = show s

-- testing
testParse parse = testFile (putStrLn . showTokens "\n" . parse)
