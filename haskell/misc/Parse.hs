import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


simple = letter

--parens = char '(' >> maybeParens >> char ')' >> maybeParens
--maybeParens = parens <|> return ()

--word = do c <- letter
--          ((word >>= return . (c:)) <|> return [c])

word = many1 letter <?> "word"

separator = skipMany1 (space <|> char ',' <?> "")

sentence = do words <- sepBy1 word separator
              oneOf ".?!" <?> "end of sentence"
              return words

run p input =
    case (parse p "" input) of
      Left err -> putStr "parse error at " >> print err
      Right x -> print x


expr = buildExpressionParser table factor
       <?> "expression"

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
        ]
    where op s f assoc = Infix (string s >> return f) assoc

factor = (do char '('
             x <- expr
             char ')'
             return x) <|> number <?> "simple expression"

number = (many1 digit >>= return . read) <?> "number"
