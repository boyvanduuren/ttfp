module SimplyTyped
  ( LambdaTerm
  , Type(..)
  , Variable
  , Context
  , varTerm
  ) where

import qualified Data.Set as Set

-- | T = V | T -> T
data Type
  = TypeVariable Char
  | Arrow Type Type
  deriving (Eq, Ord)

instance Show Type where
  show t =
    case t of
      TypeVariable v -> [v]
      Arrow t1 t2 -> "(" ++ show t1 ++ "→" ++ show t2 ++ ")"

-- | A typed variable consists of a type and an identifier
type Variable = (Type, Char)

-- | A context consists of a set of typed variables.
type Context = Set.Set Variable

data LambdaTerm
  = Var Variable
  | Ap LambdaTerm LambdaTerm
  | Ab Variable LambdaTerm

varTerm :: Context -> Variable -> Maybe LambdaTerm
varTerm c v =
  if v `Set.member` c then Just (Var v) else Nothing

instance Show LambdaTerm where
  show term =
    case term of
      Var v -> show v
      Ap x@(Var _) y@(Var _) -> show x ++ show y
      Ap t1 t2 -> "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
      Ab v t1 -> "λ" ++ show v ++ "." ++ show t1
