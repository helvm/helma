module HelVM.HelCam.Machines.ETA.Evaluator.InteractEvaluatorTest where

import HelVM.HelCam.Machines.ETA.Evaluator.InteractEvaluator
import HelVM.HelCam.Machines.ETA.EvaluatorTestData

import Test.HUnit

testsOfETAInteractEvaluator :: Test
testsOfETAInteractEvaluator = TestList
  [ "eval_hello"   ~: "Test hello."                                                      ~: hello     ~=? batchEval helloETA
  , "eval_hello2"  ~: "Test hello2."                                                     ~: hello     ~=? batchEval hello2ETA
  , "eval_hello3"  ~: "Test hello3."                                                     ~: hello     ~=? batchEval hello3ETA
  , "test_crlf"    ~: "Test whether the interpreter handles CR/LF sequences correctly."  ~: crlf      ~=? batchEval crlfETA
--  , "eva;_pip"     ~: "Copy standard input to standard output."                         ~: helloWorldExpected  ~=? batchEval helloWorld
--  , "eva;_pip2"    ~: "A smaller copy-input-to-output program."                          ~: helloWorldExpected  ~=? batchEval fascistHelloWorld
--  , "eva;_fact"    ~: "Print the recursively-calculated factorial of the number on the standard input stream."  ~: hello_WorldExpected ~=? batchEval padHelloWorld
--  , "eva;_bottles" ~: "Print the lyrics to ``99 Bottles of Beer on the Wall''."          ~: hello_WorldExpected ~=? batchEval theShortestHelloWorld
  ]
