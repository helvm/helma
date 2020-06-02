module HelVM.HelCam.WhiteSpace.ParserTest where

import HelVM.HelCam.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.WhiteSpace.Parser
import HelVM.HelCam.WhiteSpace.OperandParsers
import HelVM.HelCam.WhiteSpace.Token
import HelVM.HelCam.WhiteSpace.Instruction

import Numeric.Natural

import Test.HUnit hiding (Label)

testsOfWSParser :: Test
testsOfWSParser = TestList
  [ "testOfParseCat"          ~: "cat"                     ~: catIL          ~=? parseWSTL False catTL
  , "testOfParseHelloWorld"   ~: "helloWorld"              ~: helloWorldIL   ~=? parseWSTL False helloWorldTL
  , "testOfParseTruthMachine" ~: "testOfParseTruthMachine" ~: truthMachineIL ~=? parseWSTL False truthMachineTL
  ]
