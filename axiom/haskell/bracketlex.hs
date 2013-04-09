import Data.Maybe
import Data.List
import Data.Char

-- data Term = Bracket Name [Term]
--           | 
--             deriving Show

--type Name = String
--type BracketType = (String, String)

-- substDelims begin _ end = begin++end
-- substEnd _ _ end = end
-- substBody _ body _ = body
-- substAll begin body end = begin ++ body ++ end
-- substNone _ _ _ = []

substEnd end s = end++s
substNone _ = id

erase _ = []

data Token = Token String
           | Raw String
           | Bracket String [Token]
             deriving Show

--raw s = s
raw s = [Raw s]
escapeRaw s = case readLitChar $ '\\':s of
                [] -> error "invalid escape sequence"
                [(c, s')] -> raw $ c:s'
tokens s = [Token s] -- todo in lexer

noSpace ts = ts

type Tokenizer = String -> [Token]
type TokenFilter = [Token] -> [Token]

data Delimiter = StringDel String
               | LengthDel Int
               | EndDel
               | AltDel [Delimiter]
                 deriving Show

data Bracketer = Bracketer { --name::String,
                            begin, end::Delimiter,
                            subst::(String -> String -> String),
                            nested::[Bracketer],
                            lexer::Maybe (Either Tokenizer TokenFilter)}
--                 deriving Show

--bws b e = Bracketer b (StringDel b) (StringDel e)
bws b e = Bracketer (StringDel b) (StringDel e)
bwsn b e = bws b e substNone

tlex = Just . Left
tfilt = Just . Right

escape = Bracketer (StringDel "\\") (LengthDel 1) substNone [] $ tlex escapeRaw

common = [paren, square, curly, char, string, oper, comment, escapeNewline]

--todo remove bracketing chars (commas, braces, etc.)
comma end = Bracketer (StringDel ",") (AltDel [StringDel ",", StringDel end])
            substEnd common Nothing

paren = bwsn "(" ")" (comma ")":common) $ tfilt noSpace
square = bwsn "[" "]" (comma "]":common) $ tfilt noSpace
curly = bwsn "{" "}" (comma "}":common) $ tfilt noSpace
string = bwsn "\"" "\"" [escape] $ tlex raw
char = bwsn "'" "'" [escape] $ tlex raw
oper = bwsn "`" "`" [] Nothing

comment = bws "##" "\n" substEnd [] $ tlex erase
escapeNewline = Bracketer (StringDel "\\\n") (LengthDel 0) substNone []
                $ tlex erase

start = Bracketer (LengthDel 0) EndDel substNone [blankLine, commentLine, line]
        $ tlex tokens
line = Bracketer (LengthDel 0) (StringDel "\n") substNone (comma "\n":common)
       Nothing
blankLine = Bracketer (LengthDel 0) (StringDel "\n") substNone [] $ tlex erase
commentLine = bws "##" "\n" substNone [] $ tlex erase

--assuming lexer tokenizes one at a time for now
joinTokens _ [] ts = ts
joinTokens lexer s ts = t : joinTokens lexer s' ts
    where (t, s') = lexer s

--this is almost pointless
-- brackTokens brack s = joinTokens lexer s' ts
--     where (lexer, s', ts) = brack s

brackTokens brack s = joinTokens lexer s' ts
    where (lexer, s', s'') = brack s
          ts = brackTokens brack s''

childLex Nothing parentLex = parentLex
childLex (Just (Left lexer)) _ = lexer
childLex (Just (Right lexFilt)) parentLex = lexFilt . parentLex

bracketRule parentLex (Bracketer begin end sub nested lexer) = 
    applyRule beginRule endRule subRules clex
    where beginRule = delimRule begin substNone
          endRule = delimRule end substNone
          clex = childLex lexer parentLex
          subRules = map (bracketRule parentLex) nested

delimRule (StringDel delim) sub _ s =
    stripPrefix delim s >>= return . (sub delim)
delimRule (LengthDel len) sub l s =
    if len == l then Just $ sub "" s else Nothing
delimRule EndDel _ _ [] = Just []
delimRule EndDel _ _ _ = Nothing
delimRule (AltDel delims) sub l s = --choose first matching alternative
    listToMaybe $ catMaybes $ map (\d -> delimRule d sub l s) delims

--applyRules:: DelimRule -> [BracketRule] -> Tokenizer -> String -> Int -> String -> (String, [Token], String)
applyRules endRule rules lexer st len s = cont st len s
    where cont st len s =
              case endRule len s of
                Just r -> ([], lexer st, r)
                Nothing -> case catMaybes $ map (`uncurry` (len, s)) rules of
                             [] -> case s of
                                     --remember: appended a newline at the end
                                     [] -> error "unexpected end of stream"
                                     c:cs -> (c:s', ts, r)
                                         where (s', ts, r) = cont st (len+1) cs
                             (ts, s'):_ -> ([], lexer st++ts++ts', r)
                                 where (s'', ts', r) = cont s'' 0 s'

-- type DelimRule = Int -> String -> Maybe String
-- type BracketRule = Int -> String -> Maybe ([Token], String)

--applyRule :: DelimRule -> DelimRule -> [BracketRule] -> Tokenizer -> BracketRule
applyRule beginRule endRule subRules lexer len s =
--    let rules = map ($ lexer) subRules in
    do s' <- beginRule len s
       let (prev, ts, s'') = applyRules endRule subRules lexer prev 0 s'
       Just (ts, s'') -- todo: need a rule-specific join on ts...
