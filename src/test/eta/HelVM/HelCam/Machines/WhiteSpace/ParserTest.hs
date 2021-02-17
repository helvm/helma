module HelVM.HelCam.Machines.WhiteSpace.ParserTest where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.Machines.WhiteSpace.Parser

import Test.HUnit

testsOfWSParser :: Test
testsOfWSParser = TestList
  [ "testOfParseCat"          ~: "cat"                     ~: catIL          ~=? parseTL catTL          False
  , "testOfParseHelloWorld"   ~: "helloWorld"              ~: helloWorldIL   ~=? parseTL helloWorldTL   False
  , "testOfParseTruthMachine" ~: "testOfParseTruthMachine" ~: truthMachineIL ~=? parseTL truthMachineTL False
  ]
