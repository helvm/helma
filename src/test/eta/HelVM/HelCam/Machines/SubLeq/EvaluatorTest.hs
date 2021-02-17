module HelVM.HelCam.Machines.SubLeq.EvaluatorTest where

import HelVM.HelCam.Machines.SubLeq.Evaluator
import HelVM.HelCam.Machines.SubLeq.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfSQEvaluator :: Test
testsOfSQEvaluator = TestList
  [ "eval_hello"   ~: "Test hello."     ~: hello     ~=? batchSimpleEvalIL helloSQIL
--  , "eval_hello2"  ~: "Test hello2."  ~: hello     ~=? batchExecMockIO (eval hello2ETA)

  , "eval_hello"   ~: "Test hello."     ~: hello     ~=? batchExecMockIO (simpleEvalIL helloSQIL)
--  , "eval_hello2"  ~: "Test hello2."  ~: hello     ~=? batchExecMockIO (eval hello2ETA)
  ]
