module HelVM.HelCam.Machines.BrainFuck.Evaluator.InteractEvaluatorTest where

import HelVM.HelCam.Machines.BrainFuck.Evaluator.InteractEvaluator
import HelVM.HelCam.Machines.BrainFuck.EvaluatorTestData

import Test.HUnit

testsOfBFInteractEvaluator :: Test
testsOfBFInteractEvaluator = TestList
  [ "test_value256_forInt8"       ~: "puts '8 bit cells'"        ~: "8 bit cells\n"     ~=? batchEvalInt8  value256
  , "test_value256_forWord8"      ~: "puts '8 bit cells"         ~: "8 bit cells\n"     ~=? batchEvalWord8 value256
  , "test_helloWorld"             ~: "puts helloWorldExpected"   ~: helloWorldExpected  ~=? batchEvalWord8 helloWorld
  , "test_fascistHelloWorld"      ~: "puts helloWorldExpected"   ~: helloWorldExpected  ~=? batchEvalWord8 fascistHelloWorld
  , "test_padHelloWorld"          ~: "puts hello_WorldExpected"  ~: hello_WorldExpected ~=? batchEvalWord8 padHelloWorld
  , "test_theShortestHelloWorld"  ~: "puts hello_WorldExpected"  ~: hello_WorldExpected ~=? batchEvalWord8 theShortestHelloWorld
  ]
