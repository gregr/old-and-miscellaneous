module Value where
import Expr
import Util
import Data.Maybe
import Data.IORef
import Data.Array.IO
import qualified Data.Map as M

data Env k v = RootEnv
             | Env { parent :: Env k v, mapping :: M.Map k v }
--               deriving Show -- todo: this won't work later

envGet RootEnv _ = Nothing
envGet env k = maybe (envGet (parent env) k) Just $ M.lookup k $ mapping env
envSet RootEnv _ _ = undefined
envSet env k v = M.insert k v $ mapping env
newEnv parent = Env parent M.empty

type ValEnv = Env Sym Val

type Proc = Val -> IO Val -- more like: list -> IO Val

data Val = VTuple TupleArray -- supports tagged data, arrays, etc.
         | VSymbol Sym
         | VTag Tag
         | VChar Char
         | VInt Integer
         | VFloat Double
         | VNull
         | VProc Proc
         | VExpr (Expr Val)
         | VThunk (IORef Thunk)
--           deriving Eq

data Thunk = Delayed (Expr Val) ValEnv
           | Indirect Val

type TupleArray = (IOArray Int Val)

--showVals :: [Val] -> IO String
showVals sep vs = mapM showVal vs >>= return . join sep

showVal t@(VTuple arr) = do b <- isList t
                            if b then showValList t else showValTuple t
showVal (VSymbol s) = return s
showVal (VTag t) = return t
showVal (VChar c) = return $ show c
showVal (VInt i) = return $ show i
showVal (VFloat f) = return $ show f
showVal (VNull) = return nullStr
showVal (VProc _) = return "#procedure"
showVal (VExpr e) = showExpr e
showVal (VThunk _) = return "#thunk"

nullStr = openApp++closeApp

showValList l =
    unlist l >>= showVals " " >>= return . (surround openList closeList)
showValTuple t =
    tupleData t >>= showVals " " >>= return . (surround openApp closeApp)

tTrue = VTag "True"
tFalse = VTag "False"
tCons = VTag "Cons"
tNil = VTag "Nil"

isList d = elemTag d [tCons, tNil]
isCons d = isTag d tCons
isNil d = isTag d tNil
elemTag d tags = getTag d >>= return . (`tagElem` tags)
isTag d tag = getTag d >>= return . (tagEq tag)
tagEq (VTag t1) (VTag t2) = t1 == t2
tagElem (VTag t) ts = elem t $ map (\(VTag tt) -> tt) ts

unlist d = do
  t <- getTag d
  if tagEq t tCons
    then do
      _:v:[rest] <- tupleData d
      vs <- unlist rest
      return $ v:vs
    else if tagEq t tNil then return []
         else error "not a list"

car l = withCons l (return . head) "applied car to non-cons data"
cdr l = withCons l (return . tail) "applied cdr to non-cons data"

withCons cns f err = do t <- getTag cns
                        if tagEq t tCons then tupleData cns >>= f
                          else error err

list [] = nil
list (x:xs) = list xs >>= cons x
--cons :: Val -> Val -> IO TupleArray
cons x y = tagData tCons [x, y]
-- how annoying... actually have to provide types for these singletons
nil = tagSingle tNil
true = tagSingle tTrue
false = tagSingle tFalse

tagSingle tag = newTagged tag 0 :: IO Val

--newTuple :: Int -> IO TupleArray -- todo: fix element type after testing
newTuple size = newArray_ (0, size-1) >>= return . VTuple

tupleSize (VTuple t) = getBounds t >>= return . (+1) . snd

tupleData (VTuple t) = getElems t

--tupleGet :: TupleArray -> Int -> IO Val
tupleGet (VTuple t) i = readArray t i

tupleSet (VTuple t) i v = writeArray t i v

-- tagged data nodes
newTagged tag szData = newTuple (szData+1) >>= \d -> setTag d tag >> return d
tagData tag vs = newTagged tag (length vs) >>= \d -> setData d vs >> return d
getTag d = tupleGet d 0
setTag d tag = tupleSet d 0 tag
setData d vs = mapM_ (uncurry (tupleSet d)) (zip [1..] vs)

----------------------------------------------------------------
--Expr-related (escape Val dependencies and move to Expr.hs?)
----------------------------------------------------------------
atom v = return $ Atom v

exprs vs = mapM expr vs
expr v@(VTuple _) = isList v >>= \b -> if b then appList v else atom v
expr (VExpr e) = return e
expr v = atom v

-- list should contain 2 or more vals (see repr of Bracket)
appList l = do
  vs <- unlist l >>= mapM expr
  return $ foldl1 App vs

showExprs es = mapM showExpr es >>= return . join "\n"

showExpr (App f a) = do
  sf <- showExpr f
  sa <- showExpr a
  return $ openApp++sf ++ " @ " ++ sa++closeApp
showExpr (Case e alts defAlt) = do
  se <- showExpr e
  sd <- showExpr defAlt
  sa <- showAltList alts
  return $ openApp ++ "#case " ++ se ++ " " ++ sa ++ " " ++ sd ++ closeApp
showExpr (Atom v) = showVal v

showAltList alts = return "[alts]" -- do ss <- mapM showAlt alts
--                    return $ "[" ++ join "\n" ss ++ "]"
--showTagAlt =
