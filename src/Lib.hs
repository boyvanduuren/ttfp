module Lib
  ( subterms
  , properSubterm
  ) where

import qualified Data.MultiSet as MultiSet

-- | Constructions for Lambda terms, as per TTAFP 1.3.2
data LambdaTerm
  = Var Char
  | Ap LambdaTerm LambdaTerm
  | Ab Char LambdaTerm
  deriving (Eq)

instance Show LambdaTerm where
  show t =
    case t of
      Var v -> [v]
      Ap x@(Var _) y@(Var _) -> show x ++ show y
      Ap t1 t2 -> "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
      Ab v t1 -> "λ" ++ [v] ++ "." ++ show t1

-- | Get the subterms of a LambdaTerm, as per TTAFP 1.3.5
subterms :: LambdaTerm -> [LambdaTerm]
subterms x@(Var _) = [x]
subterms compoundTerm@(Ap t1 t2) = [compoundTerm] ++ subterms t1 ++ subterms t2
subterms compoundTerm@(Ab _ t2) = compoundTerm : subterms t2

-- | Get the proper subterm of a LambaTerm, as per TTAFP 1.3.8
-- Note that this functions is dependent on the implementation of `subterms`, as
-- it expects the first element to be the reflexive element.
properSubterm :: LambdaTerm -> LambdaTerm -> Bool
properSubterm l m = l `elem` tail (subterms m)
--freeVariables :: LambaTerm -> [Char]
