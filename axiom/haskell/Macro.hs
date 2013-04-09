module Macro where
import Value
import Expr

type MacroFunc = Val -> IO Val -- more like: list -> IO Val
type MacroEnv = Env Sym MacroFunc

getMacEnv st = undefined -- todo: first define the io state variable 'st'
getMac st (VSymbol sym) = getMacEnv st >>= return . (`envGet` sym)
getMac _ _ = return Nothing

expansions st vs = mapM (expand st) vs

expand st l@(VTuple d) = do
  b <- isCons l
  if b then do
         sym <- car l
         m <- getMac st sym
         case m of
           Nothing -> unlist l >>= expansions st >>= list
           Just mac -> do args <- cdr l
                          result <- mac args
                          expand st result
    else return l
expand _ v = return v
