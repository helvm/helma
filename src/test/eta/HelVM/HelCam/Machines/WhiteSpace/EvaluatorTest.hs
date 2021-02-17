module HelVM.HelCam.Machines.WhiteSpace.EvaluatorTest where

import HelVM.HelCam.Machines.WhiteSpace.Evaluator

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorTestData

import HelVM.HelCam.Common.MockIO
import HelVM.HelCam.Common.Util

import Test.HUnit

testsOfWSEvaluator :: Test
testsOfWSEvaluator = test
  [ "evalHelloWorld"      ~: "test evalIL Hello, world"  ~: helloWorldO ~=? batchSimpleEvalIL helloWorldIL
  , "evalTruthMachine0"   ~: "test evalIL Truth-Machine" ~: zeroO       ~=? simpleEvalIL      truthMachineIL ("0\n"        ::Input)
  , "evalCalcTL"          ~: "test evalTL Calc"          ~: calcO       ~=? simpleEvalTL      calcTL         ("-1\n"       ::Input)
  , "evalCountTL"         ~: "test evalTL Count"         ~: countO      ~=? batchSimpleEvalTL countTL
  , "evalFactTL"          ~: "test evalTL Fact"          ~: factO       ~=? simpleEvalTL      factTL         ("10\n"       ::Input)
  , "evalHanoiTL"         ~: "test evalTL Hanoi"         ~: hanoiO      ~=? simpleEvalTL      hanoiTL        ("1\n"        ::Input)
  , "evalHelloWorldTL"    ~: "test evalTL HelloWorld"    ~: helloWorldO ~=? batchSimpleEvalTL helloWorldTL
  , "evalHWorldTL"        ~: "test evalTL HWorld"        ~: hWorldO     ~=? batchSimpleEvalTL hWorldTL
  , "evalLocTestTL"       ~: "test evalTL LocTest"       ~: locTestO    ~=? simpleEvalTL      locTestTL      ("1\n2\n"     ::Input)
  , "evalNameTL"          ~: "test evalTL Name"          ~: nameO       ~=? simpleEvalTL      nameTL         ("WriteOnly\n"::Input)
  , "evalTruthMachineTL"  ~: "test evalTL TruthMachine"  ~: zeroO       ~=? simpleEvalTL      truthMachineTL ("0\n"        ::Input)

  , "evalHelloWorld"      ~: "test evalIL Hello, world"  ~: helloWorldO ~=? batchExecMockIO (simpleEvalIL helloWorldIL  )
  , "evalTruthMachine0"   ~: "test evalIL Truth-Machine" ~: zeroO       ~=? execMockIO      (simpleEvalIL truthMachineIL) ("0"          ::Input)
  , "evalCalcTL"          ~: "test evalTL Calc"          ~: calcO       ~=? execMockIO      (simpleEvalTL calcTL        ) ("-1\n"       ::Input)
  , "evalCountTL"         ~: "test evalTL Count"         ~: countO      ~=? batchExecMockIO (simpleEvalTL countTL       )
  , "evalFactTL"          ~: "test evalTL Fact"          ~: factO       ~=? execMockIO      (simpleEvalTL factTL        ) ("10\n"       ::Input)
  , "evalHanoiTL"         ~: "test evalTL Hanoi"         ~: hanoiO      ~=? execMockIO      (simpleEvalTL hanoiTL       ) ("1\n"        ::Input)
  , "evalHelloWorldTL"    ~: "test evalTL HelloWorld"    ~: helloWorldO ~=? batchExecMockIO (simpleEvalTL helloWorldTL  )
  , "evalHWorldTL"        ~: "test evalTL HWorld"        ~: hWorldO     ~=? batchExecMockIO (simpleEvalTL hWorldTL      )
  , "evalLocTestTL"       ~: "test evalTL LocTest"       ~: locTestO    ~=? execMockIO      (simpleEvalTL locTestTL     ) ("1\n2\n"     ::Input)
  , "evalNameTL"          ~: "test evalTL Name"          ~: nameO       ~=? execMockIO      (simpleEvalTL nameTL        ) ("WriteOnly\n"::Input)
  , "evalTruthMachineTL"  ~: "test evalTL TruthMachine"  ~: zeroO       ~=? execMockIO      (simpleEvalTL truthMachineTL) ("0\n"        ::Input)

  ]
