module HelVM.HelCam.Machines.ETA.EvaluatorTest where

import HelVM.HelCam.Machines.ETA.Evaluator

import HelVM.HelCam.Machines.ETA.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfETAEvaluator :: Test
testsOfETAEvaluator = test
  [ "eval_hello"   ~: "Test hello."                                                      ~: hello     ~=? batchSimpleEval helloETA
  , "eval_hello2"  ~: "Test hello2."                                                     ~: hello     ~=? batchSimpleEval hello2ETA
  , "eval_hello3"  ~: "Test hello3."                                                     ~: hello     ~=? batchSimpleEval hello3ETA
  , "test_crlf"    ~: "Test whether the interpreter handles CR/LF sequences correctly."  ~: crlf      ~=? batchSimpleEval crlfETA

  , "eval_hello"   ~: "Test hello."                                                      ~: hello     ~=? batchExecMockIO (simpleEval helloETA)
  , "eval_hello2"  ~: "Test hello2."                                                     ~: hello     ~=? batchExecMockIO (simpleEval hello2ETA)
  , "eval_hello3"  ~: "Test hello3."                                                     ~: hello     ~=? batchExecMockIO (simpleEval hello3ETA)
  , "test_crlf"    ~: "Test whether the interpreter handles CR/LF sequences correctly."  ~: crlf      ~=? batchExecMockIO (simpleEval crlfETA)

  ]
