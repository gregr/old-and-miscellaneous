module Group where
import Lex

group ts = groupTop initAttr $ cleanIndents ts

newTopBracket name attr grpNew grpCont ts = Bracket name new attr:rest
    where (new, ts') = grpNew attr ts
          rest = grpCont ts'

newBracket name attr grpNew grpCont ts = (Bracket name new attr:rest, ts'')
    where (new, ts') = grpNew attr ts
          (rest, ts'') = grpCont ts'

addToken cont (t:ts) = (t:rest, ts')
    where (rest, ts') = cont ts

groupBracket end a [] =
    srcErr a $ "unexpected end of stream while searching for: " ++ end
groupBracket end a (Newline _ _:ts) = groupBracket end a ts
groupBracket end a (Bracket b _ attr:ts) =
    if b == end then ([], ts) else
        newBracket b attr (groupBracket (matchBracket b attr))
           (groupBracket end a) ts
groupBracket end a ts = addToken (groupBracket end a) ts

groupIndent ident a [] =
    srcErr a "unexpected end of stream while searching for newline"
groupIndent indent a (Newline i' attr:ts) =
    if i' <= indent then ([], Newline i' attr:ts) else
        newBracket appBracket attr (groupIndent i') (groupIndent indent a) ts
groupIndent indent a (Bracket b _ attr:ts) =
    newBracket b attr (groupBracket $ matchBracket b attr)
                   (groupIndent indent a) ts
groupIndent indent a ts = addToken (groupIndent indent a) ts

groupTop _ [] = []
groupTop _ (Newline _ _:[]) = []
groupTop _ (Newline i attr:ts) =
    newTopBracket appBracket attr (groupIndent i) (groupTop attr) ts
groupTop a _ = srcErr a "top-level group not beginning with newline"

cleanIndents ts = dedup $ Newline 0 initAttr:ts
    where dedup [] = [Newline 0 (-1, 0)] -- row, col should never be used
          dedup (n@(Newline _ _):ts) = case ts of
                                         [] -> rest
                                         (Newline _ _:ts') -> rest
                                         _ -> n:rest
              where rest = dedup ts
          dedup (t:ts) = t:dedup ts

--testParse $ group . tokens
