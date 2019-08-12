module Main where

import Lib
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.MultiSet as MS
import qualified Data.Set as Set

-- subterm test cases
testBasicSubterm :: Test.Framework.Test
testBasicSubterm =
  testCase "term is subterm of itself" $
  MS.singleton (Var 'x') @=? subterms varTerm
  where
    varTerm = Var 'x'

testApplicationSubterm :: Test.Framework.Test
testApplicationSubterm =
  testCase "test on application" $
  MS.fromList [apTerm, Var 'x', Var 'y'] @=? subterms apTerm
  where
    apTerm = Ap (Var 'x') (Var 'y')

testAbstractionSubterm :: Test.Framework.Test
testAbstractionSubterm =
  testCase "test on abstraction" $
  MS.fromList [abTerm, varTerm] @=? subterms abTerm
  where
    varTerm = Var 'x'
    abTerm = Ab 'x' varTerm

-- properSubterm test cases
testProperSubtermNotReflexive :: Test.Framework.Test
testProperSubtermNotReflexive =
  testCase "not a proper subterm of itself" $
  False @=? properSubterm simpleTerm simpleTerm
  where
    simpleTerm = Var 'x'

testProperSubtermSuccess :: Test.Framework.Test
testProperSubtermSuccess =
  testCase "'x' is a subterm in xy" $
  True @=? properSubterm termX (Ap termX termY)
  where
    termX = Var 'x'
    termY = Var 'y'

-- freeVariables test cases
testVariableFreeVars :: Test.Framework.Test
testVariableFreeVars =
  testCase "'x' is a free variable in the term x" $
  Set.singleton 'x' @=? freeVariables (Var 'x')

testAbstractionFreeVars :: Test.Framework.Test
testAbstractionFreeVars =
  testCase "No free variables i Lx.x" $
  Set.empty @=? freeVariables (Ab 'x' (Var 'x'))

testAbstractionOverApplicationFreeVars :: Test.Framework.Test
testAbstractionOverApplicationFreeVars =
  testCase "'y' is a free variable in Lx.xy" $
  Set.singleton 'y' @=? freeVariables (Ab 'x' (Ap (Var 'x') (Var 'y')))

testAbstractionOverNestedApplicationFreeVars :: Test.Framework.Test
testAbstractionOverNestedApplicationFreeVars =
  testCase "'y' is a free variable in Lx.xxy" $
  Set.singleton 'y' @=?
  freeVariables (Ab 'x' (Ap (Ap (Var 'x') (Var 'x')) (Var 'y')))

testApplicationOverAbstractionFreeVars :: Test.Framework.Test
testApplicationOverAbstractionFreeVars =
  testCase "'x' and 'y' are free variables in x(Lx.xy)" $
  Set.fromList ['y', 'x'] @=?
  freeVariables (Ap (Var 'x') (Ab 'x' (Ap (Var 'x') (Var 'y'))))

-- isClosed test cases
testCombinatorIsClosed :: Test.Framework.Test
testCombinatorIsClosed =
  testCase "Given Lx.x, it should return true" $
  True @=? isClosed (Ab 'x' (Var 'x'))

testFreeVarIsClosed :: Test.Framework.Test
testFreeVarIsClosed =
  testCase "Given Lx.y, it should return false" $
  False @=? isClosed (Ab 'x' (Var 'y'))

-- isBound test cases
testCombinatorIsBound :: Test.Framework.Test
testCombinatorIsBound =
  testCase "'x' is bound in Lx.x" $ True @=? isBound (Ab 'x' (Var 'x')) 'x'

testNonUsedBindingIsBound :: Test.Framework.Test
testNonUsedBindingIsBound =
  testCase "'x' is bound in Lx.y" $ True @=? isBound (Ab 'x' (Var 'y')) 'x'

testFreeVarIsBound :: Test.Framework.Test
testFreeVarIsBound =
  testCase "'y' is not bound in Lx.y" $ False @=? isBound (Ab 'x' (Var 'y')) 'y'

testNestedAbstractionIsBound :: Test.Framework.Test
testNestedAbstractionIsBound =
  testCase "'y' is bound in (x(Lx.Ly.xz)" $
  True @=? isBound (Ap (Var 'x') (Ab 'x' (Ab 'y' (Ap (Var 'x') (Var 'z'))))) 'y'

-- renameVariable test cases
testRenameFreeVariable :: Test.Framework.Test
testRenameFreeVariable =
  testCase "Rename y to z in Lx.xy should result in Lx.xz" $
  expected @=? renameVariable (Ab 'x' (Ap (Var 'x') (Var 'y'))) 'y' 'z'
  where
    expected = Just (Ab 'x' (Ap (Var 'x') (Var 'z')))

testRenameBoundVariable :: Test.Framework.Test
testRenameBoundVariable =
  testCase "Rename x to y in Lx.x should result in Nothing" $
  Nothing @=? renameVariable (Ab 'x' (Var 'x')) 'x' 'y'

main :: IO ()
main =
  defaultMain
    [ testGroup
        "subterm"
        [testBasicSubterm, testApplicationSubterm, testAbstractionSubterm]
    , testGroup
        "properSubterm"
        [testProperSubtermNotReflexive, testProperSubtermSuccess]
    , testGroup
        "freeVariables"
        [ testVariableFreeVars
        , testAbstractionFreeVars
        , testAbstractionOverApplicationFreeVars
        , testAbstractionOverNestedApplicationFreeVars
        , testApplicationOverAbstractionFreeVars
        ]
    , testGroup "isClosed" [testCombinatorIsClosed, testFreeVarIsClosed]
    , testGroup
        "isBound"
        [ testCombinatorIsBound
        , testNonUsedBindingIsBound
        , testFreeVarIsBound
        , testNestedAbstractionIsBound
        ]
    , testGroup
        "renameVariable"
        [testRenameFreeVariable, testRenameBoundVariable]
    ]
