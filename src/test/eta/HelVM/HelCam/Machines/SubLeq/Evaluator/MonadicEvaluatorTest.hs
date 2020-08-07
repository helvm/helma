module HelVM.HelCam.Machines.SubLeq.Evaluator.MonadicEvaluatorTest where

import HelVM.HelCam.Machines.SubLeq.Evaluator.MonadicEvaluator
import HelVM.HelCam.Machines.SubLeq.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfSQMonadicEvaluator :: Test
testsOfSQMonadicEvaluator = TestList
  [ "eval_hello"   ~: "Test hello."     ~: hello     ~=? batchExecMockIO (evalIL helloSQIL)
--  , "eval_hello2"  ~: "Test hello2."  ~: hello     ~=? batchExecMockIO (eval hello2ETA)
  ]
