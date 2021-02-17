module HelVM.HelCam.Machines.WhiteSpace.EvaluatorTest where

import HelVM.HelCam.Machines.WhiteSpace.Evaluator

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.Common.MockIO
import HelVM.HelCam.Common.Util

import Test.HUnit

testsOfWSEvaluator :: Test
testsOfWSEvaluator = test
  [ "evalHelloWorld"      ~: "test evalIL Hello, world"  ~: helloWorldO ~=? batchEvalIL helloWorldIL
  , "evalTruthMachine0"   ~: "test evalIL Truth-Machine" ~: zeroO       ~=? evalIL      truthMachineIL       ("0\n"        ::Input)
  , "evalCalcTL"          ~: "test evalTL Calc"          ~: calcO       ~=? evalTL      False calcTL         ("-1\n"       ::Input)
  , "evalCountTL"         ~: "test evalTL Count"         ~: countO      ~=? batchEvalTL False countTL
  , "evalFactTL"          ~: "test evalTL Fact"          ~: factO       ~=? evalTL      False factTL         ("10\n"       ::Input)
  , "evalHanoiTL"         ~: "test evalTL Hanoi"         ~: hanoiO      ~=? evalTL      False hanoiTL        ("1\n"        ::Input)
  , "evalHelloWorldTL"    ~: "test evalTL HelloWorld"    ~: helloWorldO ~=? batchEvalTL False helloWorldTL
  , "evalHWorldTL"        ~: "test evalTL HWorld"        ~: hWorldO     ~=? batchEvalTL False hWorldTL
  , "evalLocTestTL"       ~: "test evalTL LocTest"       ~: locTestO    ~=? evalTL      False locTestTL      ("1\n2\n"     ::Input)
  , "evalNameTL"          ~: "test evalTL Name"          ~: nameO       ~=? evalTL      False nameTL         ("WriteOnly\n"::Input)
  , "evalTruthMachineTL"  ~: "test evalTL TruthMachine"  ~: zeroO       ~=? evalTL      False truthMachineTL ("0\n"        ::Input)

  , "evalHelloWorld"      ~: "test evalIL Hello, world"  ~: helloWorldO ~=? batchExecMockIO (evalIL helloWorldIL        )
  , "evalTruthMachine0"   ~: "test evalIL Truth-Machine" ~: zeroO       ~=? execMockIO      (evalIL truthMachineIL      ) ("0"          ::Input)
  , "evalCalcTL"          ~: "test evalTL Calc"          ~: calcO       ~=? execMockIO      (evalTL False calcTL        ) ("-1\n"       ::Input)
  , "evalCountTL"         ~: "test evalTL Count"         ~: countO      ~=? batchExecMockIO (evalTL False countTL       )
  , "evalFactTL"          ~: "test evalTL Fact"          ~: factO       ~=? execMockIO      (evalTL False factTL        ) ("10\n"       ::Input)
  , "evalHanoiTL"         ~: "test evalTL Hanoi"         ~: hanoiO      ~=? execMockIO      (evalTL False hanoiTL       ) ("1\n"        ::Input)
  , "evalHelloWorldTL"    ~: "test evalTL HelloWorld"    ~: helloWorldO ~=? batchExecMockIO (evalTL False helloWorldTL  )
  , "evalHWorldTL"        ~: "test evalTL HWorld"        ~: hWorldO     ~=? batchExecMockIO (evalTL False hWorldTL      )
  , "evalLocTestTL"       ~: "test evalTL LocTest"       ~: locTestO    ~=? execMockIO      (evalTL False locTestTL     ) ("1\n2\n"     ::Input)
  , "evalNameTL"          ~: "test evalTL Name"          ~: nameO       ~=? execMockIO      (evalTL False nameTL        ) ("WriteOnly\n"::Input)
  , "evalTruthMachineTL"  ~: "test evalTL TruthMachine"  ~: zeroO       ~=? execMockIO      (evalTL False truthMachineTL) ("0\n"        ::Input)

  ]
