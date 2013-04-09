import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Data.IORef

data Bind = Name [Name] Expr
            deriving Show

data Expr = Case Expr [Alt]
          | Let [LetBind] Expr
          | Constr Tag [Atom]
          | App Name [Atom]
          | Atom Atom
            deriving Show

type Alt = (Pattern, Expr)
type LetBind = (Name, Expr)

data Pattern = TagPat Tag [Name]
             | LitPat Literal
             | VarPat Name
             | EmptyPat
               deriving Show

data Atom = Var Name
          | Lit Literal
            deriving Show

type Literal = Int
type Tag = String
type Name = String

isTag str = isUpper $ head str

----------------------------------------------------------------
data Value = LitVal Literal
           | FuncVal Func
           | RecordVal Tag [Value]
           | AppVal Value [Value]
--           | ClosVal Closure
           | ThunkVal Thunk
           | EmptyVal -- literal or record instead?
           | UndefVal
             deriving Show

data Func = Func { params::[Name], body::Expr } deriving Show

type Thunk = Int --IORef Value

data Env = Root | Env { parent::Env, bindings::Map.Map Name Value }
           deriving Show
