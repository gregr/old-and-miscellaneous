-- todo symbols, macros, etc.
-- (remember: inner definitions have to hide macros with the same name)
-- arrays (boxed and unboxed..?)
-- making new functions

import Control.Monad
import Control.Monad.Error
import Data.IORef
--import Data.Array.ST --or IO?

data Expression = Value Value
                | Variable Symbol
                | Application Expression Expression

type Symbol = String

type Execution = Environment -> IOCanFail Box

data Value = None
           | Symbol String -- have Syntax handle this need?
           | Function (Execution -> Execution)
           | Syntax Expression
           | Bool Bool
           | Number Integer
           | String String

instance Show Value where
    show None = "None"
    show (Symbol s) = show s
    show (Function f) = "Function"
    show (Number n) = show n
    show (String s) = show s
    show (Bool b) = show b

eval (Value v) = \_ -> boxActual v
eval (Variable sym) = flip envLookup sym
eval (Application opExpr argExpr) =
    \env -> do
      box <- opExec env
      op <- unbox box
      case op of
        Function f -> apply f argExec env
        _ -> throwError $ NonApplicable $ show op
    where opExec = eval opExpr
          argExec = eval argExpr

boxActual v = lift $ makeBox $ Actual v
boxDelayed exec env = lift $ makeBox $ Delayed exec env

apply f arg = f arg

typeMismatch expected found = throwError $ TypeMismatch expected $ show found

boxNumberWith f (Number n) = boxActual $ f n
boxNumberWith _ val = typeMismatch "Number" val

boxSymbolWith f (Symbol s) = boxActual $ f s
boxSymbolWith _ val = typeMismatch "Symbol" val

unpackSyntax (Syntax s) = return s
--unpackSyntax val = typeMismatch "Syntax" val

callStrict f arg env = arg env >>= unbox >>= f
callLazy f arg env = boxDelayed arg env >>= f

primIncrease k = callStrict $ boxNumberWith $ Number . (+k)

primAdd = callStrict $ boxNumberWith $ Function . primIncrease

primPrint = callStrict $ \v -> liftIO (print v) >> boxActual None

primDefineSymbol sym arg env =
    boxDelayed arg env >>= liftIO . envDefine env sym >> boxActual None

primDefine = callStrict $ boxSymbolWith (Function . primDefineSymbol)

lazyFuncExec :: Symbol -> Execution -> Environment -> Execution -> Execution
lazyFuncExec param bodyExec env arg callingEnv = do
  thunk <- boxDelayed arg callingEnv
  innerEnv <- liftIO $ extendEnv env [param] [Just thunk]
  bodyExec innerEnv

primFunctionBody param arg env = do
  body <- unbox =<< arg env
  case body of
    Syntax s -> boxActual (Function $ lazyFuncExec param
                                        (eval =<< unpackSyntax body) env)
    val -> typeMismatch "Syntax" val

primFunction = callStrict $ boxSymbolWith (Function . primFunctionBody)

finalEval expr env = eval expr env >>= unbox

data CoreError = Default String
               | TypeMismatch String String --(Value s)
               | NonApplicable String
               | Unbound Symbol
               | Unassigned Symbol
                 deriving Show

instance Error CoreError where
    noMsg = Default "error"
    strMsg = Default

--type CanFail = Either CoreError
--errorValue action = catchError action return
-- liftCanFail (Left e) = throwError e
-- liftCanFail (Right v) = return v

type IOCanFail = ErrorT CoreError IO

runIOCanFail action handleError =
    runErrorT (action `catchError` extractError) >>= return . extractResult
        where extractResult (Right value) = value
              extractError err = return $ handleError err

data BoxedValue = Actual Value
                | Delayed Execution Environment
newtype Box = Box { boxedRef :: IORef BoxedValue}

makeBox boxVal = liftM Box $ newIORef boxVal
--getBoxedValue box = readIORef $ boxedRef box
--unbox :: Box -> IOCanFail Value
unbox (Box rVal) = liftIO (readIORef rVal) >>= unboxValue rVal
    where unboxValue _ (Actual v) = return v
          unboxValue rVal (Delayed exec env) = do
            (Box rv) <- exec env
            val <- liftIO $ readIORef rv
            liftIO $ writeIORef rVal val
            extract val
              where extract (Actual v) = return v
                    --extract _ = throwError

newtype BindingValue = BindingValue (IORef (Maybe Box))

makeBindingValue maybeValue = liftM BindingValue $ newIORef maybeValue
getBindingValue (BindingValue rMaybeVal) = readIORef rMaybeVal
setBindingValue (BindingValue rMaybeVal) val = writeIORef rMaybeVal $ Just val

type Binding = (Symbol, BindingValue)
newtype Frame = Frame (IORef [Binding])

makeFrame syms maybeVals =
  mapM makeBindingValue maybeVals >>=
  newIORef . zip syms >>=
  return . Frame

getFrameBindings (Frame rbs) = readIORef rbs
setFrameBindings (Frame rbs) bs = writeIORef rbs bs

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

data Environment = Environment { parent :: Maybe Environment,
                                   frame :: Frame }

-- note that neither this nor makeFrame compares the length of syms and vs
makeEnv parent syms vals = liftM (Environment parent) $ makeFrame syms vals
makeRootEnv syms vals = makeEnv Nothing syms vals

extendEnv env syms vals = makeEnv (Just env) syms vals
envDefine env sym val = frameDefine (frame env) sym val

envLookup :: Environment -> Symbol -> IOCanFail Box
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
-- actualValue (Just box) = liftM Just $ unbox box
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
-- t1 = makeBox $ Actual 42
-- t2 = makeBox $ Actual 54
-- test = testFrame >>= pureFrame

root = makeRootEnv [] []
t1 = makeBox $ Actual $ Number 42
t2 = makeBox $ Actual $ Number 54
test = do r <- lift root
          lift (t1 >>= envDefine r "yes")
          lift (t2 >>= envDefine r "no")
          envLookup r "yes" >>= unbox

errHandler err = String $ show err
runTest t = runIOCanFail t errHandler >>= return . show
testRun = runTest test

expr = (Value (Number 73))
expr2 = Application
        (Application (Value (Function primAdd))
         (Variable "foo"))
        (Value (Number 23))
expr3 = Application (Value (Function ($))) (Value (Number 81))
expr4 = Application (Value (Function primPrint)) (Value (Number 64))
expr5 = Application 
        (Application (Value (Function primDefine))
         (Value (Symbol "hello")))
        (Value (Number 5))
expr6 = Application 
        (Application (Value (Function primDefine))
         (Value (Symbol "world")))
        (Value (Number 7))
expr7 = Application 
        (Application (Value (Function primDefine))
         (Value (Symbol "result")))
        expr8
expr8 = Application
        (Application (Value (Function primAdd))
         (Variable "hello"))
        (Variable "world")
expr9 = Variable "result"

expr10 = Application
         (Application
          (Value (Function primFunction)) (Value (Symbol "foo")))
         (Value (Syntax expr2))

expr11 = Application expr10 (Value (Number 4))

test2 = do
  env <- lift root
  finalEval expr5 env
  finalEval expr6 env
  finalEval expr7 env
  finalEval expr9 env


testThis exp = do r <- lift root
                  finalEval exp r

result = runTest $ testThis expr4

result2 = runTest $ testThis expr3

result3 = runTest test2

result4 = runTest $ testThis expr11

-- end testing


-- data Lambda = Lambda Env Expr

-- data Function = Native (Value -> Value) | Compound Lambda

-- data Value = Number Integer | String String | Array () | Function Function


--makeArray n = newArray (0, n) 0

--setArray a i v = a ! i = v
