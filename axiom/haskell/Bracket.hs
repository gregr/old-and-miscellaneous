module Bracket where
import Operator
import Group
import Lex

-- todo: replace this with something analogous to the OpMap
-- these must correspond to existing programatic macro names
bracketMacros = [
 (appBracket, id),
 (listBracket, nameBracket "list"),
 (setOrDictBracket, nameBracket "setOrDict"),
 (hashAppBracket, nameBracket "template"), -- todo: figure this one out later
 (hashListBracket, nameBracket "array"),
 (hashSetOrDictBracket, nameBracket "hashSetOrDict")]

matchBracketMacro b a = matchBracketItem bracketMacros b a

-- by simply converting brackets into macro applications in the program, more
-- complicated decisions can be delegated to the program itself
nameBracket name (Bracket _ ts a) = Bracket appBracket (Ident name a:ts) a

applyBrackets ts = map appBracket ts
    where appBracket (Bracket b ts' a) = matchBracketMacro b a $
                                         Bracket b (applyBrackets ts') a
          appBracket t = t

-- testing
--parse = applyBrackets . assocOps testOpMap . group . tokens
