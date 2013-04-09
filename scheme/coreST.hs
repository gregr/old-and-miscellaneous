import Control.Monad
import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Data.Array.ST --or IO?

--type ResultType s = STCanFail s (Thunk s)
type Execution s = Environment s -> STCanFail s (Thunk s)

type Symbol = String
data Value s = Number Integer
             | String String
             | Function (Execution s -> Execution s)

instance Show (Value s) where
    show (Number n) = show n
    show (String s) = show s
    show (Function f) = "Function"

data Expression s = Value (Value s)
                  | Variable Symbol
                  | Application (Expression s) (Expression s)

--evall expr env = analyze expr env

eval (Value v) = \_ -> packValue v
eval (Variable sym) = flip envLookup sym
eval (Application opExpr argExpr) =
    \env -> do
      op <- evalThunk =<< opExec env
      case op of
        Function f -> apply f argExec env
        _ -> throwError $ NonApplicable $ show op
    where opExec = eval opExpr
          argExec = eval argExpr

packValue v = lift $ makeThunk $ Actual v

--unpackFunction (Function f) = f
--unpackFunction val = 

--apply (Function f) arg = arg
apply f arg = f arg

primIncrease k arg env = do
  val <- evalThunk =<< (arg env)
  case val of
    (Number n) -> packValue $ Number $ n + k
    _ -> throwError $ TypeMismatch "Number" $ show val

primAdd arg env = do
    val <- evalThunk =<< (arg env)
    case val of
      (Number n) -> packValue $ Function (primIncrease n)
      _ -> throwError $ TypeMismatch "Number" $ show val

primPrint arg env = do
  let thunk = (arg env)
  val <- evalThunk =<< thunk
  lift $ unsafeIOToST $ print val
  thunk

--apply op arg = lift $ makeThunk $ Actual op --(unpackFunction op)

-- apply op args env = (op env) --args env
-- apply op args env = evalThunk (op env) args env

strictValue thunk = evalThunk thunk >> return thunk

--finalEval expr env = eval expr env >>= strictValue
finalEval expr env = do
  thunk <- eval expr env
  evalThunk thunk

data CoreError s = Default String
                 | TypeMismatch String String --(Value s)
                 | NonApplicable String
                 | Unbound Symbol
                 | Unassigned Symbol
                   deriving Show

instance Error (CoreError s) where
    noMsg = Default "error"
    strMsg = Default

type CanFail s = Either (CoreError s)

errorValue action = catchError action return
extractValue (Right value) = value
liftCanFail (Left e) = throwError e
liftCanFail (Right v) = return v

type STCanFail s = ErrorT (CoreError s) (ST s)

runSTCanFail action handleError =
    runErrorT (action `catchError` extractError) >>= return . extractValue
        where extractError err = return $ handleError err
--   r <- runErrorT action
--   case r of
--     Left e -> return errCode
--     Right v -> return v


--data ThunkValue s = Actual Value | Delayed Expression (Environment s)
data ThunkValue s = Actual (Value s)
                  | Delayed (Environment s -> ThunkValue s) (Environment s)
newtype Thunk s = Thunk (STRef s (ThunkValue s))

makeThunk thunkVal = liftM Thunk $ newSTRef thunkVal

evalThunk (Thunk rVal) = lift (readSTRef rVal) >>= evalThunkValue rVal

evalThunkValue _ (Actual v) = return v
evalThunkValue rVal (Delayed latentValue env) = do
  let val = latentValue env
  lift $ writeSTRef rVal val
  evalThunkValue rVal val

type MaybeValue s = Maybe (Thunk s)

newtype BindingValue s = BindingValue (STRef s (MaybeValue s))

makeBindingValue maybeValue = liftM BindingValue $ newSTRef maybeValue
getBindingValue (BindingValue rMaybeVal) = readSTRef rMaybeVal
setBindingValue (BindingValue rMaybeVal) val = writeSTRef rMaybeVal $ Just val

type Binding s = (Symbol, BindingValue s)
newtype Frame s = Frame (STRef s [Binding s])

makeFrame syms maybeVals =
  mapM makeBindingValue maybeVals >>=
  newSTRef . zip syms >>=
  return . Frame

getFrameBindings (Frame rbs) = readSTRef rbs
setFrameBindings (Frame rbs) bs = writeSTRef rbs bs

frameLookup frame sym = liftM (lookup sym) $ getFrameBindings frame

frameBind frame sym val = do
  v <- makeBindingValue (Just val)
  bs <- getFrameBindings frame
  setFrameBindings frame $ (sym, v):bs

frameDefine frame sym val = do
  bs <- getFrameBindings frame
  maybe (frameBind frame sym val)
        (flip setBindingValue val)
        (lookup sym bs)

type MaybeEnv s = Maybe (Environment s)
data Environment s = Environment { parent :: MaybeEnv s,
                                   frame :: Frame s }

-- note that neither this nor makeFrame compares the length of syms and vs
makeEnv parent syms vals = liftM (Environment parent) $ makeFrame syms vals
makeRootEnv syms vals = makeEnv Nothing syms vals

extendEnv env syms vals = makeEnv (Just env) syms vals
envDefine env sym val = frameDefine (frame env) sym val

envLookup :: Environment s -> Symbol -> STCanFail s (Thunk s)
envLookup env sym = envLookup' $ Just env
    where envLookup' (Just env) = do
            mbv <- lift $ frameLookup (frame env) sym
            case mbv of
              Nothing -> envLookup' (parent env)
              Just bv -> do maybeVal <- lift $ getBindingValue bv
                            case maybeVal of
                              Just val -> return val
                              Nothing -> throwError $ Unassigned sym
          envLookup' Nothing = throwError $ Unbound sym

-- for testing
-- actualValue (Just thunk) = liftM Just $ evalThunk thunk
-- actualValue Nothing = return Nothing

-- mapFrameBindings f = liftM (map f) . getFrameBindings
-- frameSymbols = mapFrameBindings fst
-- frameBindingValues = mapFrameBindings snd
-- frameValues frame = mapM getBindingValue =<< frameBindingValues frame
-- pureFrame frame = do
--   syms <- frameSymbols frame
--   vals <- mapM actualValue =<< frameValues frame
--   return $ zip syms vals

-- testFrame = makeFrame ["one", "two"] [Nothing, Nothing]
-- t1 = makeThunk $ Actual 42
-- t2 = makeThunk $ Actual 54
-- test = testFrame >>= pureFrame

root = makeRootEnv [] []
t1 = makeThunk $ Actual $ Number 42
t2 = makeThunk $ Actual $ Number 54
test = do r <- lift root
          lift (t1 >>= envDefine r "yes")
          lift (t2 >>= envDefine r "no")
          envLookup r "yes" >>= evalThunk

errHandler err = String $ show err
runTest t = runSTCanFail t errHandler >>= return . show
testRun = runTest test
result = runST testRun

expr = (Value (Number 73))
expr2 :: Expression s
expr2 = (Application
         (Application (Value (Function primAdd))
          (Value (Number 47)))
         (Value (Number 23)))
expr3 = (Application (Value (Function ($))) (Value (Number 81)))
expr4 = (Application (Value (Function primPrint)) (Value (Number 64)))

testThis exp = do r <- lift root
                  finalEval exp r
test2 = testThis expr4

testRun2 = runTest test2
result2 = runST testRun2

-- end testing


-- data Lambda = Lambda Env Expr

-- data Function = Native (Value -> Value) | Compound Lambda

-- data Value = Number Integer | String String | Array () | Function Function

-- data Expr = Value Value | Variable Symbol | Application [Expr]



makeArray n = newArray (0, n) 0

--setArray a i v = a ! i = v
