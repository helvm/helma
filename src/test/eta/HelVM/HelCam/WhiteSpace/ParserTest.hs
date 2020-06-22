module HelVM.HelCam.WhiteSpace.ParserTest where

import HelVM.HelCam.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.WhiteSpace.Parser

import Test.HUnit

testsOfWSParser :: Test
testsOfWSParser = TestList
  [ "testOfParseCat"          ~: "cat"                     ~: catIL          ~=? parseWSTL False catTL
  , "testOfParseHelloWorld"   ~: "helloWorld"              ~: helloWorldIL   ~=? parseWSTL False helloWorldTL
  , "testOfParseTruthMachine" ~: "testOfParseTruthMachine" ~: truthMachineIL ~=? parseWSTL False truthMachineTL
  ]
