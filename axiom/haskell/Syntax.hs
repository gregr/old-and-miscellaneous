module Syntax where
import Bracket
import Operator
import Group
import Lex
import Macro
import Value
import Util

-- todo: pass on source attributes?
reprs ts = mapM repr ts

repr (Bracket b ts _) = if b == appBracket then
                            case ts of
                              [] -> return VNull
                              [t] -> repr t
                              _ -> reprs ts >>= list
                        else error $ "repr of non-app bracket: " ++ b
repr (Ident n _) = return $ VSymbol n
repr (Oper n _) = return $ VSymbol n
repr (Lit v _) = reprLit v

reprLit (IntVal i) = return $ VInt i
reprLit (FloatVal f) = return $ VFloat f
reprLit (CharVal c) = return $ VChar c
reprLit (StringVal s) = (list $ map VChar s) >>= quoteList

quoteList l = list [VSymbol "quote", l]

--testing
testSyntaxParse = applyBrackets . assocOps testOpMap . group . tokens
testReprs parse = testFile (\s -> reprs (parse s) >>= showVals "\n" >>= putStrLn)
runReprs = testReprs testSyntaxParse

testExprs parse = testFile (\s -> reprs (parse s) >>= exprs >>= showExprs >>= putStrLn)
runExprs = testExprs testSyntaxParse

-- (code is still data in its intermediate repr until after macro expansion)
testMacExpand = expansions undefined --todo: define state variable
testMacReprs parse = testFile (\s -> reprs (parse s) >>= testMacExpand >>= showVals "\n" >>= putStrLn)
runMacReprs = testMacReprs testSyntaxParse

testMacExprs parse = testFile (\s -> reprs (parse s) >>= testMacExpand >>= exprs >>= showExprs >>= putStrLn)
runMacExprs = testMacExprs testSyntaxParse
