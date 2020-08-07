module HelVM.HelCam.Machines.SubLeq.Evaluator.InteractEvaluatorTest where

import HelVM.HelCam.Machines.SubLeq.Evaluator.InteractEvaluator
import HelVM.HelCam.Machines.SubLeq.EvaluatorTestData

import Test.HUnit

testsOfSQInteractEvaluator :: Test
testsOfSQInteractEvaluator = TestList
  [ "eval_hello"   ~: "Test hello."     ~: hello     ~=? batchEvalIL helloSQIL
--  , "eval_hello2"  ~: "Test hello2."  ~: hello     ~=? batchExecMockIO hello2ETA
  ]
