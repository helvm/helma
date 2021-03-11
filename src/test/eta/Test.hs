module Main(main) where

import HelVM.HelCam.Common.FilterIf0Test

import HelVM.HelCam.Machines.BrainFuck.TokensTest
import HelVM.HelCam.Machines.BrainFuck.EvaluatorTest

import HelVM.HelCam.Machines.ETA.LexerTest
import HelVM.HelCam.Machines.ETA.EvaluatorUtilTest
import HelVM.HelCam.Machines.ETA.EvaluatorTest

import HelVM.HelCam.Machines.SubLeq.EvaluatorTest

import HelVM.HelCam.Machines.WhiteSpace.OperandParsersTest
import HelVM.HelCam.Machines.WhiteSpace.ParserTest
import HelVM.HelCam.Machines.WhiteSpace.EvaluatorTest

import Test.HUnit

testExample :: Test
testExample = TestCase (assertEqual ("test"::String) ("test"::String) ("test"::String))

testList :: Test
testList = TestList
  [ TestLabel "testExample"                 testExample
  , TestLabel "testsOfFilterIf0"            testsOfFilterIf0

  , TestLabel "testsOfBFTokens"             testsOfBFTokens
  , TestLabel "testsOfBFEvaluator"          testsOfBFEvaluator

  , TestLabel "testsOfETATokens"            testsOfETATokens
  , TestLabel "testsOfETAEvaluatoUtil"      testsOfETAEvaluatorUtil
  , TestLabel "testsOfETAEvaluator"         testsOfETAEvaluator

  , TestLabel "testsOfSQEvaluator"          testsOfSQEvaluator

  , TestLabel "testsOfWSOperandParsers"     testsOfWSOperandParsers
  , TestLabel "testsOfWSParser"             testsOfWSParser
  , TestLabel "testsOfWSEvaluator"          testsOfWSEvaluator
  ]

main :: IO ()
main = do
  _ <- runTestTT testList
  pass
