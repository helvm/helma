module HelVM.HelCam.Machines.WhiteSpace.ParserTest where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.Machines.WhiteSpace.Parser

import Test.HUnit

testsOfWSParser :: Test
testsOfWSParser = TestList
  [ "testOfParseCat"          ~: "cat"                     ~: catIL          ~=? parseWSTL False catTL
  , "testOfParseHelloWorld"   ~: "helloWorld"              ~: helloWorldIL   ~=? parseWSTL False helloWorldTL
  , "testOfParseTruthMachine" ~: "testOfParseTruthMachine" ~: truthMachineIL ~=? parseWSTL False truthMachineTL
  ]
