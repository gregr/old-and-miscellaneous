import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language (haskellStyle)

lexer = makeTokenParser haskellStyle{ reservedOpNames = ["*", "/", "+", "-"] }

expr = buildExpressionParser table factor
       <?> "expression"

table = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
        ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
        ]
    where op s f assoc =
              Infix ((reservedOp lexer s >> return f) <?> "operator") assoc

factor = parens lexer expr <|> natural lexer <?> "simple expression"

run p input =
    case (parse p "" input) of
      Left err -> putStr "parse error at " >> print err
      Right x -> print x

runLex p = run (do whiteSpace lexer
                   x <- p
                   eof
                   return x)
