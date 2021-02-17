module HelVM.HelCam.Machines.WhiteSpace.Evaluator.MonadicEvaluatorTest where

import HelVM.HelCam.Machines.WhiteSpace.Evaluator.MonadicEvaluator

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfWSMonadicEvaluator :: Test
testsOfWSMonadicEvaluator = test
  [ "evalHelloWorld"    ~: "test evalIL Hello, world"  ~: "Hello, world"   ~=? batchExecMockIO (evalIL helloWorldIL        )
  , "evalTruthMachine0" ~: "test evalIL Truth-Machine" ~: "0"              ~=? execMockIO      (evalIL truthMachineIL      ) "0"
  , "evalCalcTL"          ~: "test evalTL Calc"          ~: calcO          ~=? execMockIO      (evalTL False calcTL        ) "-1\n"
  , "evalCountTL"         ~: "test evalTL Count"         ~: countO         ~=? batchExecMockIO (evalTL False countTL       )
  , "evalFactTL"          ~: "test evalTL Fact"          ~: factO          ~=? execMockIO      (evalTL False factTL        ) "10\n"
  , "evalHanoiTL"         ~: "test evalTL Hanoi"         ~: hanoiO         ~=? execMockIO      (evalTL False hanoiTL       ) "1\n"
  , "evalHelloWorldTL"    ~: "test evalTL HelloWorld"    ~: "Hello, world" ~=? batchExecMockIO (evalTL False helloWorldTL  )
  , "evalHWorldTL"        ~: "test evalTL HWorld"        ~: hWorldO        ~=? batchExecMockIO (evalTL False hWorldTL      )
  , "evalLocTestTL"       ~: "test evalTL LocTest"       ~: locTestO       ~=? execMockIO      (evalTL False locTestTL     ) "1\n2\n"
  , "evalNameTL"          ~: "test evalTL Name"          ~: nameO          ~=? execMockIO      (evalTL False nameTL        ) "WriteOnly\n"
  , "evalTruthMachineTL"  ~: "test evalTL TruthMachine"  ~: "0"            ~=? execMockIO      (evalTL False truthMachineTL) "0\n"
  ]
