module HelVM.HelCam.Machines.ETA.Evaluator.MonadicEvaluatorTest where

import HelVM.HelCam.Machines.ETA.Evaluator.MonadicEvaluator
import HelVM.HelCam.Machines.ETA.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfETAMonadicEvaluator :: Test
testsOfETAMonadicEvaluator = TestList
  [ "eval_hello"   ~: "Test hello."                                                      ~: hello     ~=? batchExecMockIO (eval helloETA)
  , "eval_hello2"  ~: "Test hello2."                                                     ~: hello     ~=? batchExecMockIO (eval hello2ETA)
  , "eval_hello3"  ~: "Test hello3."                                                     ~: hello     ~=? batchExecMockIO (eval hello3ETA)
  , "test_crlf"    ~: "Test whether the interpreter handles CR/LF sequences correctly."  ~: crlf      ~=? batchExecMockIO (eval crlfETA)
--  , "eva;_pip"     ~: "Copy standard input to standard output."                         ~: helloWorldExpected  ~=? batchExecMockIO (evalWord8 helloWorld)
--  , "eva;_pip2"    ~: "A smaller copy-input-to-output program."                          ~: helloWorldExpected  ~=? batchExecMockIO (evalWord8 fascistHelloWorld)
--  , "eva;_fact"    ~: "Print the recursively-calculated factorial of the number on the standard input stream."  ~: hello_WorldExpected ~=? batchExecMockIO (evalWord8 padHelloWorld)
--  , "eva;_bottles" ~: "Print the lyrics to ``99 Bottles of Beer on the Wall''."          ~: hello_WorldExpected ~=? batchExecMockIO (evalWord8 theShortestHelloWorld)
  ]
