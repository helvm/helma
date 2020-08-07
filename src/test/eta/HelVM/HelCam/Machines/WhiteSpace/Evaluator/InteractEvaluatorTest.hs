module HelVM.HelCam.Machines.WhiteSpace.Evaluator.InteractEvaluatorTest where

import HelVM.HelCam.Machines.WhiteSpace.Evaluator.InteractEvaluator

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorTestData

import Test.HUnit

testsOfWSInteractEvaluator :: Test
testsOfWSInteractEvaluator = test
  [ "evalHelloWorld"     ~: "test evalIL Hello, world"  ~: "Hello, world" ~=? batchEvalIL helloWorldIL
  , "evalTruthMachine0"  ~: "test evalIL Truth-Machine" ~: "0"            ~=? evalIL      truthMachineIL       "0\n"
  , "evalCalcTL"         ~: "test evalTL Calc"          ~: calcO          ~=? evalTL      False calcTL        "-1\n"
  , "evalCountTL"        ~: "test evalTL Count"         ~: countO         ~=? batchEvalTL False countTL
  , "evalFactTL"         ~: "test evalTL Fact"          ~: factO          ~=? evalTL      False factTL         "10\n"
  , "evalHanoiTL"        ~: "test evalTL Hanoi"         ~: hanoiO         ~=? evalTL      False hanoiTL        "1\n"
  , "evalHelloWorldTL"   ~: "test evalTL HelloWorld"    ~: "Hello, world" ~=? batchEvalTL False helloWorldTL
  , "evalHWorldTL"       ~: "test evalTL HWorld"        ~: hWorldO        ~=? batchEvalTL False hWorldTL
  , "evalLocTestTL"      ~: "test evalTL LocTest"       ~: locTestO       ~=? evalTL      False locTestTL      "1\n2\n"
  , "evalNameTL"         ~: "test evalTL Name"          ~: nameO          ~=? evalTL      False nameTL         "WriteOnly\n"
  , "evalTruthMachineTL" ~: "test evalTL TruthMachine"  ~: "0"            ~=? evalTL      False truthMachineTL "0\n"
  ]
