module Expr where

-- final executable representation
data Expr v = App (Expr v) (Expr v)
          | Case (Expr v) (AltList v) (Expr v)
          | Atom v -- includes Literals, Aggregates, Symbols, Functions, etc.
--            deriving Eq
--            deriving Show

--a group of alternatives must have the same pattern type
data AltList v = TagAlts [Alt v TagPat]
               | CharAlts [Alt v CharPat]
               | IntAlts [Alt v IntPat]
               | FloatAlts [Alt v FloatPat]
--               deriving Eq
--               deriving Show

type Alt v p = (p, Expr v)
type TagPat = (Tag, [Sym])
type CharPat = Char
type IntPat = Integer
type FloatPat = Double

type Tag = String
type Sym = String
