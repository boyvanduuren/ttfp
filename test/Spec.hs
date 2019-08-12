module Main where

import Lib
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.MultiSet as MS
import qualified Data.Set as Set

-- subterm test cases
testBasicSubterm =
  testCase "subterm - term is subterm of itself" $
  (MS.singleton $ Var 'x') @=? (subterms varTerm)
  where
    varTerm = Var 'x'

testApplicationSubterm =
  testCase "subterm - test on application" $
  (MS.fromList [apTerm, Var 'x', Var 'y']) @=? (subterms apTerm)
  where
    apTerm = Ap (Var 'x') (Var 'y')

testAbstractionSubterm =
  testCase "subterm - test on abstraction" $
  (MS.fromList [abTerm, varTerm]) @=? (subterms abTerm)
  where
    varTerm = Var 'x'
    abTerm = Ab 'x' varTerm

-- properSubterm test cases
testProperSubtermNotReflexive =
  testCase "properSubterm - not a proper subterm of itself" $
  False @=? (properSubterm simpleTerm simpleTerm)
  where
    simpleTerm = Var 'x'

testProperSubtermSuccess =
  testCase "properSubterm - success case" $
  True @=? (properSubterm termX $ Ap termX termY)
  where
    termX = Var 'x'
    termY = Var 'y'

-- freeVariables test cases
testVariableFreeVars =
  testCase "Free variables in 'x' is ['x']" $
  (Set.singleton 'x') @=? (freeVariables $ Var 'x')

testAbstractionFreeVars =
  testCase "freeVariables - Given Lx.x return []" $
  (Set.empty) @=? (freeVariables $ Ab 'x' $ Var 'x')

testAbstractionOverApplicationFreeVars =
  testCase "freeVariables - Given Lx.xy return ['y']" $
  (Set.singleton 'y') @=? (freeVariables $ Ab 'x' $ Ap (Var 'x') (Var 'y'))

testAbstractionOverNestedApplicationFreeVars =
  testCase "freeVariables - Given Lx.xxy return ['y']" $
  (Set.singleton 'y') @=?
  (freeVariables $ Ab 'x' $ Ap (Ap (Var 'x') (Var 'x')) (Var 'y'))

testApplicationOverAbstractionFreeVars =
  testCase "freeVariables - Given x(Lx.xy) return ['x', 'y']" $
  (Set.fromList ['y', 'x']) @=?
  (freeVariables $ Ap (Var 'x') (Ab 'x' (Ap (Var 'x') (Var 'y'))))

main :: IO ()
main =
  defaultMainWithOpts
    [ testBasicSubterm
    , testApplicationSubterm
    , testAbstractionSubterm
    , testProperSubtermNotReflexive
    , testProperSubtermSuccess
    , testVariableFreeVars
    , testAbstractionFreeVars
    , testAbstractionOverApplicationFreeVars
    , testAbstractionOverNestedApplicationFreeVars
    , testApplicationOverAbstractionFreeVars
    ]
    mempty
