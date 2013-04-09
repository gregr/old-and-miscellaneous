module Operator where
import Group
import Lex
import Data.Maybe
import qualified Data.Map as M

-- for constructing rhs infix sections (ie. (+1)): assumes existence of flip
flipIdent = Ident "flip" (-1, -1)

type OpInfo = Maybe (Bool, Int)
type OpMap = M.Map Name OpInfo

newOpMap = M.empty
addPrefixOp ops name = tryInsert ops name Nothing
addInfixOp ops name rassoc prec = tryInsert ops name (Just (rassoc, prec))
-- todo: many wasteful multiple lookups will occur in assocOps...
isInfix ops n = isJust $ getInfo ops n
isRightAssoc ops n = maybe (error $ "isRightAssoc on prefix operator: " ++ n)
                     fst $ getInfo ops n
opPrec ops n = maybe (error $ "opPrec on prefix operator: " ++ n) snd
               $ getInfo ops n

getInfo ops name = fromMaybe (error $ "looked up unknown operator: " ++ name)
                   $ M.lookup name ops
tryInsert ops name info = if M.notMember name ops then M.insert name info ops
                          else error $ "operator redeclaration: " ++ name

opCmp ops (Oper n1 _) (Oper n2 _) =
    prec2 > prec1 || (prec2 == prec1 && isRightAssoc ops n2)
    where prec1 = opPrec ops n1
          prec2 = opPrec ops n2

assocOps ops ts = findOp [] $ map recurse ts
    where recurse (Bracket b ts' a) = Bracket b (assocOps ops ts') a
          recurse t = t

          findOp [] op@[(Oper _ _)] = op
          findOp [] (op@(Oper n a):ts) =
              if isInfix ops n then
                  let (rhs, ts') = infixOp op [] ts in
                  if null ts' then [makeAppAttr [flipIdent, op, rhs] a]
                  else error "bad infix op section"
              else appPrefix findOp [] op ts
          findOp lhs [op@(Oper n _)] =
              if isInfix ops n then [makeApp [op, (makeApp $ reverse lhs)]]
              else error "bad prefix op section"
          findOp lhs (op@(Oper n _):ts) =
              if isInfix ops n then
                  let lhs' = makeApp $ reverse lhs
                      (rhs, ts') = infixOp op [] ts in
                  findOp [makeApp [op, lhs', rhs]] ts'
              else appPrefix findOp lhs op ts
          findOp lhs (t:ts) = findOp (t:lhs) ts
          findOp lhs [] = reverse lhs

          appPrefix cont acc op ts = cont (next:acc) ts'
              where (next, ts') = prefixOp op ts

          prefixOp op [] = error "bad prefix op section"
          prefixOp op (next@(Oper n _):ts) =
              if isInfix ops n then error "prefix op followed by infix op"
              else (makeApp [op, rhs], ts')
                  where (rhs, ts') = prefixOp next ts
          prefixOp op (t:ts) = (makeApp [op, t], ts)

          infixOp op rhs xs@(next@(Oper n _):ts) =
              if isInfix ops n then
                  case rhs of
                    [] -> error "adjacent infix ops"
                    _ -> if opCmp ops op next then
                             case ts of
                               [] -> error "bad infix op section"
                               ts -> infixOp op [makeApp [next, lhs, rhs']] ts'
                                   where (rhs', ts') = infixOp next [] ts
                                         lhs = makeApp $ reverse rhs
                         else (makeApp $ reverse rhs, xs)
              else appPrefix (infixOp op) rhs next ts
          infixOp op rhs (t:ts) = infixOp op (t:rhs) ts
          infixOp op rhs [] = (makeApp $ reverse rhs, [])

          makeAppAttr [arg] _ = arg -- avoid superfluous bracketing
          makeAppAttr args attr = Bracket appBracket args attr
          makeApp args = makeAppAttr args $ getAttr $ head args

-- testing
testPrefixOps = ["~", "$", "!", "?"]
testInfixOps = [
 (",", (False, 0)),
 ("+", (False, 5)),
 ("*", (False, 6)),
 ("++", (True, 3)),
 (":", (True, 4))]

testOpMap = foldl (\ops (n, (ra, prec)) -> addInfixOp ops n ra prec)
            (foldl addPrefixOp newOpMap testPrefixOps) testInfixOps

--testParse $ assocOps testOpMap . group . tokens
