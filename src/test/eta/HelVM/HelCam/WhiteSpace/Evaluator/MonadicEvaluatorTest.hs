module HelVM.HelCam.WhiteSpace.Evaluator.MonadicEvaluatorTest where

import HelVM.HelCam.WhiteSpace.Evaluator.MonadicEvaluator

import HelVM.HelCam.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfWSMonadicEvaluator :: Test
testsOfWSMonadicEvaluator = test
  [ "evalWSHelloWorld"    ~: "test evalWSIL Hello, world"  ~: "Hello, world" ~=? batchExecMockIO (evalWSIL helloWorldIL        )
  , "evalWSTruthMachine0" ~: "test evalWSIL Truth-Machine" ~: "0"            ~=? execMockIO      (evalWSIL truthMachineIL      ) "0"
  , "evalCalcTL"          ~: "test evalWSTL Calc"          ~: calcO          ~=? execMockIO      (evalWSTL False calcTL        ) "-1\n"
  , "evalCountTL"         ~: "test evalWSTL Count"         ~: countO         ~=? batchExecMockIO (evalWSTL False countTL       )
  , "evalFactTL"          ~: "test evalWSTL Fact"          ~: factO          ~=? execMockIO      (evalWSTL False factTL        ) "10\n"
  , "evalHanoiTL"         ~: "test evalWSTL Hanoi"         ~: hanoiO         ~=? execMockIO      (evalWSTL False hanoiTL       ) "1\n"
  , "evalHelloWorldTL"    ~: "test evalWSTL HelloWorld"    ~: "Hello, world" ~=? batchExecMockIO (evalWSTL False helloWorldTL  )
  , "evalHWorldTL"        ~: "test evalWSTL HWorld"        ~: hWorldO        ~=? batchExecMockIO (evalWSTL False hWorldTL      )
  , "evalLocTestTL"       ~: "test evalWSTL LocTest"       ~: locTestO       ~=? execMockIO      (evalWSTL False locTestTL     ) "1\n2\n"
  , "evalNameTL"          ~: "test evalWSTL Name"          ~: nameO          ~=? execMockIO      (evalWSTL False nameTL        ) "WriteOnly\n"
  , "evalTruthMachineTL"  ~: "test evalWSTL TruthMachine"  ~: "0"            ~=? execMockIO      (evalWSTL False truthMachineTL) "0\n"
  ]
