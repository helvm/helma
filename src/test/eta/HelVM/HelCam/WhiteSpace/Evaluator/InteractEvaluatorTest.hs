module HelVM.HelCam.WhiteSpace.Evaluator.InteractEvaluatorTest where

import HelVM.HelCam.WhiteSpace.Evaluator.InteractEvaluator
import HelVM.HelCam.WhiteSpace.Instruction
import HelVM.HelCam.WhiteSpace.ParserTest

import HelVM.HelCam.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Control.Monad.State.Lazy

import Test.HUnit

testsOfWSInteractEvaluator :: Test
testsOfWSInteractEvaluator = test
  [ "evalWSHelloWorld"    ~: "test evalWSIL Hello, world"  ~: "Hello, world" ~=? batchEvalWSIL helloWorldIL
  , "evalWSTruthMachine0" ~: "test evalWSIL Truth-Machine" ~: "0"            ~=? evalWSIL      truthMachineIL       "0\n"
  , "evalCalcTL"          ~: "test evalWSTL Calc"          ~: calcO          ~=? evalWSTL      False calcTL        "-1\n"
  , "evalCountTL"         ~: "test evalWSTL Count"         ~: countO         ~=? batchEvalWSTL False countTL
  , "evalFactTL"          ~: "test evalWSTL Fact"          ~: factO          ~=? evalWSTL      False factTL         "10\n"
  , "evalHanoiTL"         ~: "test evalWSTL Hanoi"         ~: hanoiO         ~=? evalWSTL      False hanoiTL        "1\n"
  , "evalHelloWorldTL"    ~: "test evalWSTL HelloWorld"    ~: "Hello, world" ~=? batchEvalWSTL False helloWorldTL
  , "evalHWorldTL"        ~: "test evalWSTL HWorld"        ~: hWorldO        ~=? batchEvalWSTL False hWorldTL
  , "evalLocTestTL"       ~: "test evalWSTL LocTest"       ~: locTestO       ~=? evalWSTL      False locTestTL      "1\n2\n"
  , "evalNameTL"          ~: "test evalWSTL Name"          ~: nameO          ~=? evalWSTL      False nameTL         "WriteOnly\n"
  , "evalTruthMachineTL"  ~: "test evalWSTL TruthMachine"  ~: "0"            ~=? evalWSTL      False truthMachineTL "0\n"
  ]
