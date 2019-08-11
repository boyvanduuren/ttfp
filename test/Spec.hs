module Main where

import Lib
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.MultiSet as MS

-- subterm test cases
testBasicSubterm =
  assertEqual "Basis for subterms" (MS.singleton $ Var 'x') (subterms varTerm)
  where
    varTerm = Var 'x'

testApplicationSubterm =
  assertEqual
    "Application for subterms"
    (MS.fromList [apTerm, Var 'x', Var 'y'])
    (subterms apTerm)
  where
    apTerm = Ap (Var 'x') (Var 'y')

testAbstractionSubterm =
  assertEqual
    "Abstraction for subterms"
    (MS.fromList [abTerm, varTerm])
    (subterms abTerm)
  where
    varTerm = Var 'x'
    abTerm = Ab 'x' varTerm

-- properSubterm test cases
testProperSubtermNotReflexive =
  assertEqual
    "A lambda term is not a proper subterm of itself"
    False
    (properSubterm simpleTerm simpleTerm)
  where
    simpleTerm = Var 'x'

testProperSubtermSuccess =
  assertEqual
    "When MN, N is a proper subterm of M"
    True
    (properSubterm termX $ Ap termX termY)
  where
    termX = Var 'x'
    termY = Var 'y'

-- freeVariables test cases
testVariableFreeVars =
  assertEqual
    "A 'Var' lambda term always returns its constructor argument"
    (MS.singleton 'x')
    (freeVariables $ Var 'x')

testAbstractionFreeVars =
  assertEqual
    "A bound variable can never be free"
    (MS.empty)
    (freeVariables $ Ab 'x' $ Var 'x')

testAbstractionOverApplicationFreeVars =
  assertEqual
    "Given Lx.xy return ['y']"
    (MS.singleton 'y')
    (freeVariables $ Ab 'x' $ Ap (Var 'x') (Var 'y'))

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "subterm - basic" testBasicSubterm
    , testCase "subterm - application" testApplicationSubterm
    , testCase "subterm - abstraction" testAbstractionSubterm
    , testCase
        "properSubterm - not a proper subterm of itself"
        testProperSubtermNotReflexive
    , testCase "properSubterm - success case" testProperSubtermSuccess
    , testCase "Free variables in 'x' is ['x']" testVariableFreeVars
    , testCase
        "A variable bound in an abstraction should not be returned as free"
        testAbstractionFreeVars
    , testCase "Given Lx.xy return ['y']" testAbstractionOverApplicationFreeVars
    ]
    mempty
