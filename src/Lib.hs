module Lib
  ( LambdaTerm(..)
  , subterms
  , properSubterm
  , freeVariables
  ) where

import qualified Data.MultiSet as MS
import qualified Data.Set as Set

-- | Constructions for Lambda terms, as per TTFP 1.3.2
data LambdaTerm
  = Var Char
  | Ap LambdaTerm LambdaTerm
  | Ab Char LambdaTerm
  deriving (Eq, Ord)

instance Show LambdaTerm where
  show t =
    case t of
      Var v -> [v]
      Ap x@(Var _) y@(Var _) -> show x ++ show y
      Ap t1 t2 -> "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
      Ab v t1 -> "λ" ++ [v] ++ "." ++ show t1

-- | Get the subterms of a LambdaTerm, as per TTFP 1.3.5
subterms :: LambdaTerm -> MS.MultiSet LambdaTerm
subterms x@(Var _) = MS.singleton x
subterms compoundTerm@(Ap t1 t2) =
  MS.insert compoundTerm $ MS.union (subterms t1) (subterms t2)
subterms compoundTerm@(Ab _ t2) = MS.insert compoundTerm (subterms t2)

-- | Get the proper subterm of a LambaTerm, as per TTFP 1.3.8
-- Note that this functions is dependent on the implementation of `subterms`, as
-- it expects the first element to be the reflexive element.
properSubterm :: LambdaTerm -> LambdaTerm -> Bool
properSubterm l m = subterms l `MS.isProperSubsetOf` subterms m

-- | Get the free variables from a lambda term as defined in TTFP 1.4.1
freeVariables :: LambdaTerm -> Set.Set Char
freeVariables (Var x) = Set.singleton x
freeVariables (Ap t1 t2) = Set.union (freeVariables t1) (freeVariables t2)
freeVariables (Ab x t) = Set.delete x $ freeVariables t