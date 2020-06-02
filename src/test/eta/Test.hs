module Main(main) where

import HelVM.HelCam.Common.FilterIf0Test
import HelVM.HelCam.BrainFuck.TokensTest
import HelVM.HelCam.BrainFuck.Evaluator.InteractEvaluatorTest
import HelVM.HelCam.BrainFuck.Evaluator.MonadicEvaluatorTest
import HelVM.HelCam.WhiteSpace.OperandParsersTest
import HelVM.HelCam.WhiteSpace.ParserTest
import HelVM.HelCam.WhiteSpace.Evaluator.InteractEvaluatorTest
import HelVM.HelCam.WhiteSpace.Evaluator.MonadicEvaluatorTest

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
