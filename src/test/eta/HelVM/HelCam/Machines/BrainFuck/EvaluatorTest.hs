module HelVM.HelCam.Machines.BrainFuck.EvaluatorTest where

import HelVM.HelCam.Machines.BrainFuck.Evaluator
import HelVM.HelCam.Machines.BrainFuck.EvaluatorTestData

import HelVM.HelCam.Common.MockIO

import Test.HUnit

testsOfBFEvaluator :: Test
testsOfBFEvaluator = TestList
  [ "test_value256_forInt8"       ~: "puts '8 bit cells'"        ~: "8 bit cells\n"     ~=? batchEvalInt8  value256
  , "test_value256_forWord8"      ~: "puts '8 bit cells"         ~: "8 bit cells\n"     ~=? batchEvalWord8 value256
  , "test_helloWorld"             ~: "puts helloWorldExpected"   ~: helloWorldExpected  ~=? batchEvalWord8 helloWorld
  , "test_fascistHelloWorld"      ~: "puts helloWorldExpected"   ~: helloWorldExpected  ~=? batchEvalWord8 fascistHelloWorld
  , "test_padHelloWorld"          ~: "puts hello_WorldExpected"  ~: hello_WorldExpected ~=? batchEvalWord8 padHelloWorld
  , "test_theShortestHelloWorld"  ~: "puts hello_WorldExpected"  ~: hello_WorldExpected ~=? batchEvalWord8 theShortestHelloWorld

  , "test_value256_forInt8"       ~: "puts '8 bit cells'"        ~: "8 bit cells\n"     ~=? batchExecMockIO (evalInt8  value256)
  , "test_value256_forWord8"      ~: "puts '8 bit cells"         ~: "8 bit cells\n"     ~=? batchExecMockIO (evalWord8 value256)
  , "test_helloWorld"             ~: "puts helloWorldExpected"   ~: helloWorldExpected  ~=? batchExecMockIO (evalWord8 helloWorld)
  , "test_fascistHelloWorld"      ~: "puts helloWorldExpected"   ~: helloWorldExpected  ~=? batchExecMockIO (evalWord8 fascistHelloWorld)
  , "test_padHelloWorld"          ~: "puts hello_WorldExpected"  ~: hello_WorldExpected ~=? batchExecMockIO (evalWord8 padHelloWorld)
  , "test_theShortestHelloWorld"  ~: "puts hello_WorldExpected"  ~: hello_WorldExpected ~=? batchExecMockIO (evalWord8 theShortestHelloWorld)
  ]
