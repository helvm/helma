module HelVM.HelCam.Machines.WhiteSpace.ParserTest where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.Machines.WhiteSpace.Parser

import Test.HUnit

testsOfWSParser :: Test
testsOfWSParser = TestList
  [ "testOfParseCat"          ~: "cat"                     ~: catIL          ~=? parseTL False catTL
  , "testOfParseHelloWorld"   ~: "helloWorld"              ~: helloWorldIL   ~=? parseTL False helloWorldTL
  , "testOfParseTruthMachine" ~: "testOfParseTruthMachine" ~: truthMachineIL ~=? parseTL False truthMachineTL
  ]
