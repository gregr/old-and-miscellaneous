import qualified Data.Map as Map
import Data.Maybe

type Prog = [Binding]

data Binding = Binding Name [Name] Expr
               deriving Show

data Expr = Seq Term Pattern Expr
          | Case Val [Alt]
          | Term Term
            deriving Show

data Term = App Name [Atom]
          | Unit Val
          | Store Val
          | Fetch Name
          | FetchIndex Name Int
          | Update Name Val
          | Expr Expr
            deriving Show

data Val = Node Atom [Atom]
         | Atom Atom
           deriving Show

data Atom = Var Name
          | Literal Lit
          | Tag TagName
          | Empty
            deriving Show

data Alt = Alt Pattern Expr
           deriving Show

data Pattern = AtomPat Atom
             | TagNodePat TagName [Name]
             | VarNodePat [Name]
               deriving Show

type Lit = Int -- todo: expand later
type TagName = Name -- todo:
type Name = String

----------------------------------------------------------------
data Value = LitVal Lit
           | TagVal TagName
           | NodeVal Node
           | LocVal Loc
           | EmptyVal
           | UndefVal
             deriving Show

type Loc = Int
type Node = [Value] -- first item must be a tag

newNode tag vals = (TagVal tag):vals
getNodeIndex node n = node !! n

data Env = Root | Env { parent::Env, bindings::Map.Map Name Value }
           deriving Show

newEnv bs = Env Root $ Map.fromList bs
insertEnv env (n, v) = env{bindings=Map.insert n v (bindings env)}
extendEnv env bs = foldl insertEnv env bs
getEnvVal (Env p bs) name = case Map.lookup name bs of
                              Just v -> checkVal name v
                              Nothing -> getEnvVal p name
getEnvVal Root name = error $ "Unbound Variable: " ++ name
checkVal name UndefVal = error $ "Accessed undefined variable: " ++ name
checkVal name v = v

getValue env (Node a as) =
    NodeVal $ (getAtomValue env a):(map (getAtomValue env) as)
getValue env (Atom a) = getAtomValue env a
getAtomValue env (Var n) = getEnvVal env n
getAtomValue env (Literal l) = LitVal l
getAtomValue env (Tag t) = TagVal t
getAtomValue env Empty = EmptyVal

data Heap = Heap { nextLoc::Loc, nodes::Map.Map Loc Node }
            deriving Show

newHeap = Heap 0 Map.empty
newLoc (Heap nl ns) = (nl, (Heap (nl+1) ns))
newLocs (Heap nl ns) n = ([nl..nl+n-1], (Heap (nl+n) ns))
setHeapLoc h l nd = h{nodes=Map.insert l nd $ nodes h}
getHeapLoc h l = fromJust $ Map.lookup l $ nodes h

storeNode h env val = (LocVal loc, setHeapLoc h' loc nd)
    where (loc, h') = newLoc h
          NodeVal nd = getValue env val
fetchNode h env var = getHeapLoc h loc
    where LocVal loc = (getEnvVal env var)
updateNode h env var val = setHeapLoc h loc nd
    where LocVal loc = getEnvVal env var
          NodeVal nd = getValue env val

chooseAlt v [] _ = error $ "case match failure: " ++ (show v)
chooseAlt v ((Alt p e):as) env = case matchPat v p env of
                                   Just env' -> (e, env')
                                   Nothing -> chooseAlt v as env

matchPat val (AtomPat a) env = matchAtomPat val a env
matchPat val (TagNodePat t vars) env = matchTagNodePat val t vars env
matchPat val (VarNodePat vars) env = matchVarNodePat val vars env

matchVarNodePat (NodeVal vals) vars env =
    if dlen < 0 then error $ "too few vars in pattern: " ++ (show vars) ++
                             ", " ++ (show vals)
    else Just $ extendEnv env $ zip vars vals'
    where vals' = vals ++ (take dlen $ repeat UndefVal)
          dlen = length vars - length vals
matchVarNodePat _ _ _ = Nothing

matchTagNodePat (NodeVal ((TagVal t1):vals)) t2 vars env = matchConst t1 t2 env'
    where env' = extendEnv env $ zip vars vals
matchTagNodePat _ _ _ _ = Nothing

matchAtomPat (LitVal l1) (Literal l2) env = matchConst l1 l2 env
matchAtomPat (TagVal t1) (Tag t2) env = matchConst t1 t2 env
matchAtomPat EmptyVal Empty env = Just env
matchAtomPat val (Var var) env = Just $ insertEnv env (var, val)
matchAtomPat _ _ _ = Nothing

matchConst p1 p2 env = if p1 == p2 then Just env else Nothing

type Func = Globals -> Heap -> [Value] -> IO (Value, Heap)

makeFunc vars expr = \gs h vals -> let env = newEnv $ zip vars vals
                                   in eval gs h env expr
bindFunc (Binding n vs e) = (n, (makeFunc vs e))

data Globals = Globals (Map.Map Name Func)

newGlobals prims bs = foldl setGlobal (Globals Map.empty)
                      (prims ++ (map bindFunc bs))
setGlobal (Globals gs) (n, func) = Globals $ Map.insert n func gs
getGlobal (Globals gs) name = fromJust $ Map.lookup name gs

----------------------------------------------------------------
eval gs h env (Seq term pat expr) = do
  (val, h') <- evalTerm gs h env term
  env' <- return $ fromJust $ matchPat val pat env
  eval gs h' env' expr

eval gs h env (Case val alts) = eval gs h env' expr
    where (expr, env') = chooseAlt (getValue env val) alts env
eval gs h env (Term term) = evalTerm gs h env term

evalTerm gs h env (App fn as) = func gs h $ map (getAtomValue env) as
    where func = getGlobal gs fn
evalTerm _ h env (Unit val) = return (getValue env val, h)
evalTerm _ h env (Store val) = return $ storeNode h env val
evalTerm _ h env (Fetch var) = return (NodeVal (fetchNode h env var), h)
evalTerm _ h env (FetchIndex var i) = return (getNodeIndex node i, h)
    where node = fetchNode h env var
evalTerm _ h env (Update var val) = return (EmptyVal, updateNode h env var val)
evalTerm gs h env (Expr expr) = eval gs h env expr

makeState prims binds = (newGlobals prims binds, newHeap, newEnv [])
runExpr (gs, h, env) expr = eval gs h env expr
runMain state = runExpr state $ Term $ App "main" []
runProg prims binds = runMain $ makeState prims binds

----------------------------------------------------------------
--testing
----------------------------------------------------------------
makeCaf name expr = Binding name [] expr
prog = runProg [] [testMain]

--main = unit 42
--testMain = makeCaf "main" $ Term $ Unit $ Atom $ Literal $ 42

--main = unit 42; \hi -> unit hi
-- testMain = makeCaf "main" $ Seq
--            (Unit $ Atom $ Literal $ 42)
--            (AtomPat $ Var "hi")
--            (Term $ Unit $ Atom $ Var "hi")

--main = unit (CSome 5 4 3); \t a b c d e -> unit (CThing c b t a)
-- testMain = makeCaf "main" $ Seq
--            (Unit $ Node (Tag "CSome") $ map Literal [5, 4, 3])
--            (VarNodePat ["t", "a", "b", "c", "d", "e"])
--            (Term $ Unit $ Node (Tag "CThing") $ map Var ["c", "b", "t", "a"])

--main = store (CBlah 5 42 3); \loc -> fetchIndex loc 2
testMain = makeCaf "main" $ Seq
           (Store $ Node (Tag "CBlah") $ map Literal [5, 42, 3])
           (AtomPat $ Var "loc")
           (Term $ FetchIndex "loc" 2)
