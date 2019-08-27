module Untyped
  ( LambdaTerm(..)
  , subterms
  , properSubterm
  , freeVariables
  , isClosed
  , isBound
  , renameVariable
  , alphaConvert
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

-- | Returns whether a given LambdaTerm is closed or not.
-- Closed being defined as a LambdaTerm that has no free variables.
isClosed :: LambdaTerm -> Bool
isClosed = Set.null . freeVariables

-- | Returns whether a given name is used as a binding in an abstraction.
isBound :: LambdaTerm -> Char -> Bool
isBound (Var _) _ = False
isBound (Ap t1 t2) name = isBound t1 name || isBound t2 name
isBound (Ab x t) name = (x == name) || isBound t name

-- | Renames a free variable in a LambdaTerm.
renameVariable :: LambdaTerm -> Char -> Char -> Maybe LambdaTerm
renameVariable t x y =
  if isBound t x
    then Nothing
    else Just (constructReplacedTerm t x y)
  where
    constructReplacedTerm :: LambdaTerm -> Char -> Char -> LambdaTerm
    constructReplacedTerm (Var z) x' y' =
      if z == x'
        then Var y'
        else Var z
    constructReplacedTerm (Ap t1 t2) x' y' =
      Ap (constructReplacedTerm t1 x' y') (constructReplacedTerm t2 x' y')
    constructReplacedTerm (Ab z t1) x' y' =
      Ab z (constructReplacedTerm t1 x' y')

-- | Alpha conversion for lambda terms. Note that this is only defined
-- for abstractions and will return Nothing for variable and application terms.
alphaConvert :: LambdaTerm -> Char -> Char -> Maybe LambdaTerm
alphaConvert t@(Ab _ s) x y =
  if isBound s x || (not . Set.member x) (freeVariables s)
    then Nothing
    else doAlphaConversion t x y
  where
    doAlphaConversion :: LambdaTerm -> Char -> Char -> Maybe LambdaTerm
    doAlphaConversion (Ab z subterm) x' y' = do
      renamedTerm <- renameVariable subterm x' y'
      return (Ab (if x' == z then y' else z) renamedTerm)
    doAlphaConversion _ _ _ = Nothing
alphaConvert _ _ _ = Nothing