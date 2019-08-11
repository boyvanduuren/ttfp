module Main where

import Lib
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.MultiSet as MS

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

main :: IO ()
main =
  defaultMainWithOpts
    [ testCase "subterm - basic" testBasicSubterm
    , testCase "subterm - application" testApplicationSubterm
    , testCase "subterm - abstraction" testAbstractionSubterm
    ]
    mempty
