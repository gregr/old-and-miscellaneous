import qualified Data.Map as Map
import Data.Maybe

type Prog = [Binding]

data Binding = Binding { name::Name, closure::Closure }
            deriving Show

data Closure = Closure { update :: UpdateFlag,
                         freeVars :: [Name], boundVars :: [Name], rhs :: Expr,
                         freeVals :: [Value] }
               deriving Show

type UpdateFlag = Bool

data Expr = Let [Binding] Expr
          | LetRec [Binding] Expr
          | Case Expr [Alt]
          | App Name [Atom]
          | Constr Name [Atom]
          | Constant Literal
          | PrimAdd Atom Atom
          | PrimMul Atom Atom
            deriving Show

data Alt = AlgAlt Constructor [Name] Expr
         | PrimAlt Literal Expr
         | DefVarAlt Name Expr
         | DefEmptyAlt Expr
           deriving Show

data Atom = Var Name | Lit Literal
            deriving Show

data Constructor = AlgCon Name | PrimCon Literal
                   deriving Show

type Name = String
type Literal = Int
type Address = Int

-- can't construct infinite type... haskell's imperfection
--type PrimOp = [Literal] -> Literal
-- applyPrim p [] = p
-- applyPrim p (x:xs) = applyPrim (p x) xs

----------------------------------------------------------------
data State = State { code :: Code,
                     args :: [Value], returns :: [Continuation],
                     updates :: [Update], heap :: Heap, globals :: Env }
             deriving Show

data Code = Eval Expr Env
          | Enter Address -- note: read about eval/apply model for performance
          | ReturnCon Name [Value]
          | ReturnPrimVal Literal
          | Halt Value
            deriving Show

type Continuation = ([Alt], Env)
type Update = ([Value], [Continuation], Address)

----------------------------------------------------------------
exec s = execCodes s $ code s

showClosure h c = show c ++ "\n" ++ show h

execCodes s (Halt (Addr addr)) = showClosure (heap s) $ getClosure (heap s) addr
execCodes s (Halt (PrimVal k)) = show k -- ++ "\n" ++ (show (heap s))
execCodes s c = exec s'
    where s' = execCode s c

execCode s (Enter a) =
    if not shouldUpdate then
        let argLen = length as in
        if len <= argLen then
            s{code=(Eval expr env), args=(drop len (args s))}
        else
            let c' = Closure False (fvars++(take argLen bvars))
                     (drop argLen bvars) expr (fvals++as) in
            case updates s of
              (args', rets', addr):us ->
                  let as' = as ++ args'
                      h' = updateHeap h addr c' in
                  s{code=(Enter a), args=as', returns=rets', heap=h'}
              [] -> s{code=(Halt (Addr addr)), heap=h'}
                  where (addr, h') = insertHeapClosure h c'
    else
        if shouldUpdate && len == 0
        then let upd = ((args s), (returns s), a) in
             s{code=(Eval expr env), args=[], returns=[],
               updates=upd:(updates s)}
        else error "updateable closures should have no bvars"
    where env = newEnv (globals s) $ (zip fvars fvals) ++ (zip bvars bvals)
          as = args s
          len = length bvars
          bvals = take len as --should only eval when len <= argLen
          h = heap s
          (Closure shouldUpdate fvars bvars expr fvals) =
              getClosure h a

execCode s (Eval (App f as) env) =
    case getVal env (Var f) of
      Addr a -> s{code=(Enter a), args=(map (getVal env) as)++(args s)}
      PrimVal k -> case as of
                     [] -> s{code=(ReturnPrimVal k)}
                     otherwise -> error "primitive op?"

execCode s (Eval (Let bs expr) env) = s{code=(Eval expr env'), heap=h'}
   where (bs', h') = extendHeap (heap s) (allocBinds env bs)
         env' = newEnv env bs'

-- ugly duplication (note difference in allocBinds call)
execCode s (Eval (LetRec bs expr) env) = s{code=(Eval expr env'), heap=h'}
   where (bs', h') = extendHeap (heap s) (allocBinds env' bs)
         env' = newEnv env bs'

execCode s (Eval (Case expr alts) env) =
    s{code=(Eval expr env), returns=((alts, env):(returns s))}

execCode s (Eval (Constr c xs) env) =
    s{code=(ReturnCon c (map (getVal env) xs))}

execCode s (Eval (Constant k) env) = s{code=(ReturnPrimVal k)}

execCode s (Eval (PrimAdd x y) env) = s{code=applyPrim2 env (+) x y}
execCode s (Eval (PrimMul x y) env) = s{code=applyPrim2 env (*) x y}

execCode s (ReturnCon c vs) =
    case returns s of
      [] -> let c' = newClosure (AlgCon c) vs in
            case updates s of
              (args', rets', addr):us ->
                  s{code=(ReturnCon c vs), args=args', returns=rets',
                          updates=us, heap=h'}
                  where h' = updateHeap h addr c'
              [] -> s{code=(Halt (Addr addr)), heap=h'}
                  where (addr, h') = insertHeapClosure h c'
      r:rs -> s{code=code', returns=rs, heap=h'}
          where (code', h') = execReturn (AlgCon c) vs r h
      where h = heap s

execCode s (ReturnPrimVal k) =
    case returns s of
      [] -> case updates s of
              [] -> s{code=(Halt (PrimVal k))}
              _ -> error $ "cannot update with primitive value: " ++ show k
      r:rs -> s{code=code', returns=rs, heap=h'}
          where (code', h') = execReturn (PrimCon k) [] r (heap s)

execReturn c vs (alts, env) h = ((Eval expr env'), h')
    where (expr, env', h') = chooseAlt c env vs h alts

applyPrim2 env op x y = ReturnPrimVal $ op x' y'
    where PrimVal x' = getVal env x
          PrimVal y' = getVal env y

----------------------------------------------------------------
chooseAlt c env vs h alts =
    head $ catMaybes $ map (matchAlt c env vs h) alts

matchAlt (AlgCon c) env vs h (AlgAlt (AlgCon ac) xs expr) =
    if c == ac then Just (expr, newEnv env (zip xs vs), h)
    else Nothing
matchAlt (AlgCon _) _ _ _ (PrimAlt _ _) = Nothing
matchAlt (PrimCon c) env _ h (PrimAlt k expr) =
    if c == k then Just (expr, env, h)
    else Nothing
matchAlt (PrimCon c) _ _ h (AlgAlt _ _ _) = Nothing
matchAlt (PrimCon k) env _ h (DefVarAlt v expr) =
    Just (expr, newEnv env [(v, (PrimVal k))], h)
matchAlt (AlgCon c) env vs h (DefVarAlt x expr) =
    Just (expr, newEnv env [(x, addr)], h')
        where ((_, addr), h') = insertHeap h
                                (Binding x (newClosure (AlgCon c) vs))
matchAlt _ env _ h (DefEmptyAlt expr) = Just (expr, env, h)

newClosure (AlgCon c) vs = Closure False fvars [] (Constr c (map Var fvars)) vs
    where fvars = (genNames (length vs))
newClosure (PrimCon k) _ = Closure False [] [] (Constant k) []

genNames n = ["a" ++ c | c <- map show [0..n-1]]

----------------------------------------------------------------
allocBinds env bs = map (allocBind env) bs

allocBind env b = b{closure=allocClosure env (closure b)}

allocClosure env c = c{freeVals=fvals}
    where fvals = map (getEnvVal env) fvars
          fvars = freeVars c

----------------------------------------------------------------
data Value = Addr Address | PrimVal Literal
           deriving Show

getVal env (Var n) = getEnvVal env n
getVal _ (Lit v) = PrimVal v

----------------------------------------------------------------
data Heap = Heap { nextAddr::Int, addrToClosure::Map.Map Address Closure }
          deriving Show

data Env = Root | Env { parent::Env, bindings::Map.Map Name Value }
         deriving Show

getEnvVal (Env p bs) name = case Map.lookup name bs of
                              Just addr -> addr
                              Nothing -> getEnvVal p name
getEnvVal Root name = error $ "Unbound Variable: " ++ name

getClosure (Heap _ cs) addr = fromJust $ Map.lookup addr cs

----------------------------------------------------------------
insertHeap h (Binding n c) = ((n, Addr addr), h')
    where (addr, h') = insertHeapClosure h c

insertHeapClosure h c = (addr, h{nextAddr=addr+1, addrToClosure=aToC})
    where aToC = Map.insert addr c $ addrToClosure h
          addr = nextAddr h

extendHeap h bs = (map fst results, snd (last results))
    where results = extendHeap' h bs
          extendHeap' h [] = []
          extendHeap' h (b:bs) = ((name, addr), h') : extendHeap' h' bs
              where ((name, addr), h') = insertHeap h b

updateHeap h addr c = h{addrToClosure=(Map.insert addr c (addrToClosure h))}

-- entryName would normally be "main" in a haskell program
newState bs entryName = State (Enter entry) [] [] [] h g
    where (nbs, h) = extendHeap newHeap bs
          g = newGlobals nbs
          (Addr entry) = getEnvVal g entryName

newHeap = Heap 0 $ Map.fromList []

newEnv p bs = Env p $ Map.fromList bs

newGlobals bs = newEnv Root bs

--testing
makeCaf expr = Closure False [] [] expr []
testName = "main"
testState = newState testMapFold testName
test = exec testState
{-
----------------------------------------------------------------
map f [] = []
map f (y:ys) = (f y) : (map f ys)
----------------------------------------------------------------
map = {} \n {f,xs} ->
         case xs of
           Nil {} -> Nil {}
           Cons {y, ys} -> let fy = {f, y} \u {} -> f {y}
                               mfy = {f, ys} \u {} -> map {f, ys}
                           in Cons {fy, mfy}
----------------------------------------------------------------
Alternative
----------------------------------------------------------------
map1 f = mf where mf [] = []
                  mf (y:ys) = (f y) : (mf ys)
----------------------------------------------------------------
map1 = {} \n {f} ->
       letrec
         mf = {f, mf} \n {xs} ->
              case xs of
                Nil {} -> Nil {}
                Cons {y, ys} -> let fy = {f, y} \u {} -> f {y}
                                    mfy = {mf, ys} \u {} -> mf {ys}
                                in Cons {fy, mfy}
       in mf
-}

-- todo: can't use updating with unboxed values flying around
testMap = Closure False [] ["f", "xs"] testMapExpr []
testMapExpr = Case (App "xs" [])
              [AlgAlt (AlgCon "Nil") [] (Constr "Nil" []),
               AlgAlt (AlgCon "Cons") ["y", "ys"]
               (Let [Binding "fy" (Closure False ["f", "y"] []
                                   (App "f" [Var "y"]) []),
                     Binding "mfy" (Closure False ["f", "ys"] []
                                    (App "map" [Var "f", Var "ys"]) [])]
                (Constr "Cons" [Var "fy", Var "mfy"]))]
{- --todo: LetRec is actually broken due to allocClosure's behavior
foldr = {} \n {f, acc, xs} ->
           case xs of
             Nil {} -> acc
             Cons {y, ys} -> let acc' = {f, acc, y, ys} \u {} ->
                               let r = {f, acc, ys} \u {} -> foldr {f, acc, ys}
                               in f {y, r}
                             in acc'

foldl = {} \n {f, acc, xs} ->
           case xs of
             Nil {} -> acc
             Cons {y, ys} -> let acc' = {f, acc, y} \u {} -> f {acc, y}
                             in let r = {f, acc', ys} \u {} ->
                                                            foldl {f, acc', ys}
                                in r
-}

-- todo: see above about updates
testFoldl = Closure False [] ["f", "acc", "xs"] testFoldlExpr []
testFoldlExpr = Case (App "xs" [])
                [AlgAlt (AlgCon "Nil") [] (App "acc" []),
                 AlgAlt (AlgCon "Cons") ["y", "ys"]
                 (Let [Binding "acc'"
                       (Closure False ["f", "acc", "y"] []
                        (App "f" [Var "acc", Var "y"]) [])]
                  (Let [Binding "r"
                        (Closure False ["f", "acc'", "ys"] []
                         (App "foldl" [Var "f", Var "acc'", Var "ys"]) [])]
                   (App "r" [])))]

testFoldr = Closure False [] ["f", "acc", "xs"] testFoldrExpr []
testFoldrExpr = Case (App "xs" [])
                [AlgAlt (AlgCon "Nil") [] (App "acc" []),
                 AlgAlt (AlgCon "Cons") ["y", "ys"]
                 (Let [Binding "acc'"
                       (Closure False ["f", "acc", "y", "ys"] []
                        (Let [Binding "r"
                              (Closure False ["f", "acc", "ys"] []
                               (App "foldr" [Var "f", Var "acc", Var "ys"]) [])]
                         (App "f" [Var "y", Var "r"])) [])]
                  (App "acc'" []))]
{-
main = foldr (+) 0 $ map (2*) [3, 4, 5]
----------------------------------------------------------------
double = {} \n {n} -> case n of --force evaluation
                        n' -> case 2 * n' of r -> r
nil = {} \n {} -> Nil {}
xs0 = {} \n {} -> Cons {5, nil}
xs1 = {} \n {} -> Cons {4, xs0}
xs = {} \n {} -> Cons {3, xs1}
mapped = {} \n {} -> map {double, xs}
add = {} \n {a, b} -> case a of --force eval
                        a' -> case b of
                                b' -> case a' + b' of r -> r
main = {} \n {} -> foldr {add, 0, mapped}
-}
testMapFold = [Binding "map" testMap, Binding "foldr" testFoldr,
               Binding "double" (Closure False [] ["n"]
                                 (Case (App "n" [])
                                  [DefVarAlt "n'"
                                   (Case (PrimMul (Lit 2) (Var "n'"))
                                    [DefVarAlt "r" (App "r" [])])]) []),
               Binding "nil" (makeCaf (Constr "Nil" [])),
               Binding "xs0" (makeCaf (Constr "Cons" [Lit 5, Var "nil"])),
               Binding "xs1" (makeCaf (Constr "Cons" [Lit 4, Var "xs0"])),
               Binding "xs" (makeCaf (Constr "Cons" [Lit 3, Var "xs1"])),
               Binding "add" (Closure False [] ["a", "b"]
                              (Case (App "a" [])
                               [DefVarAlt "a'"
                                (Case (App "b" [])
                                 [DefVarAlt "b'"
                                  (Case (PrimAdd (Var "a'") (Var "b'"))
                                   [DefVarAlt "r" (App "r" [])])])]) []),
               Binding "mapped" (makeCaf
                                 (App "map" [Var "double", Var "xs"])),
               Binding testName (makeCaf (App "foldr" [Var "add", Lit 0,
                                                       Var "mapped"]))]
{-
----------------------------------------------------------------
(f x) + y
...
data Int = MkInt Int#

case (f x) of
  MkInt x# -> case y of
                MkInt y# -> case (x# +# y#) of
                              r# -> MkInt r#
... similar lazy CPS style ...
f x (\fx. force y (\yr. + fx yr (\r. k r)))
----------------------------------------------------------------
-}
testBoxingBs = [Binding testName $ makeCaf testBoxedProg]
testUnboxedProg = Let [Binding "bob" (makeCaf (Constant 45)),
                       Binding "rob" (makeCaf (Constant 32))]
                  (Case (App "bob" [])
                   [DefVarAlt "x" (Case (App "rob" [])
                                   [DefVarAlt "y"
                                    (PrimAdd (Var "x") (Var "y"))])])

testBoxedProg = Let [Binding "bob" (makeCaf (Constr "MkInt" [Lit 42])),
                     Binding "rob" (makeCaf (Constr "MkInt" [Lit 33]))]
                (Case (App "bob" [])
                 [AlgAlt (AlgCon "MkInt") ["x"] --unbox
                  (Case (App "rob" [])
                   [AlgAlt (AlgCon "MkInt") ["y"] --unbox
--                    (PrimAdd (Var "x") (Var "y"))])]) --test unboxed directly
                    (Case (PrimAdd (Var "x") (Var "y"))
                     [DefVarAlt "xy" (Constr "MkInt" [(Var "xy")])])])]) --rebox
