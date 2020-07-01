module Main(main) where

import HelVM.HelCam.Common.FilterIf0Test

import HelVM.HelCam.Machines.BrainFuck.TokensTest
import HelVM.HelCam.Machines.BrainFuck.Evaluator.InteractEvaluatorTest
import HelVM.HelCam.Machines.BrainFuck.Evaluator.MonadicEvaluatorTest

import HelVM.HelCam.Machines.WhiteSpace.OperandParsersTest
import HelVM.HelCam.Machines.WhiteSpace.ParserTest
import HelVM.HelCam.Machines.WhiteSpace.Evaluator.InteractEvaluatorTest
import HelVM.HelCam.Machines.WhiteSpace.Evaluator.MonadicEvaluatorTest

import Test.HUnit

testExample :: Test
testExample = TestCase (assertEqual "test" "test" "test")

testList :: Test
testList = TestList
  [ TestLabel "testExample" testExample
  , TestLabel "testsOfFilterIf0" testsOfFilterIf0
  , TestLabel "testsOfBFTokens" testsOfBFTokens
  , TestLabel "testsOfBFInteractEvaluator" testsOfBFInteractEvaluator
  , TestLabel "testsOfBFMonadicEvaluator"  testsOfBFMonadicEvaluator
  , TestLabel "testsOfWSOperandParsers" testsOfWSOperandParsers
  , TestLabel "testsOfWSParser" testsOfWSParser
  , TestLabel "testsOfWSInteractEvaluator" testsOfWSInteractEvaluator
  , TestLabel "testsOfWSMonadicEvaluator"  testsOfWSMonadicEvaluator
  ]

main :: IO ()
main = do
  _ <- runTestTT testList
  return ()
