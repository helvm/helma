module HelVM.HelCam.Machines.ETA.EvaluatorTest where

import HelVM.HelCam.Machines.ETA.Evaluator

import HelVM.HelCam.Machines.ETA.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfETAEvaluator :: Test
testsOfETAEvaluator = test
  [ "eval_hello"   ~: "Test hello."                                                      ~: hello     ~=? batchEval helloETA
  , "eval_hello2"  ~: "Test hello2."                                                     ~: hello     ~=? batchEval hello2ETA
  , "eval_hello3"  ~: "Test hello3."                                                     ~: hello     ~=? batchEval hello3ETA
  , "test_crlf"    ~: "Test whether the interpreter handles CR/LF sequences correctly."  ~: crlf      ~=? batchEval crlfETA

  , "eval_hello"   ~: "Test hello."                                                      ~: hello     ~=? batchExecMockIO (eval helloETA)
  , "eval_hello2"  ~: "Test hello2."                                                     ~: hello     ~=? batchExecMockIO (eval hello2ETA)
  , "eval_hello3"  ~: "Test hello3."                                                     ~: hello     ~=? batchExecMockIO (eval hello3ETA)
  , "test_crlf"    ~: "Test whether the interpreter handles CR/LF sequences correctly."  ~: crlf      ~=? batchExecMockIO (eval crlfETA)

  ]
