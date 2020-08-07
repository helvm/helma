module HelVM.HelCam.Machines.SubLeq.Evaluator.InteractEvaluator where

import HelVM.HelCam.Machines.SubLeq.EvaluatorUtil
import HelVM.HelCam.Machines.SubLeq.Lexer

import HelVM.HelCam.Common.Tape
import HelVM.HelCam.Common.Util

import Data.Char

interactEval :: Source -> IO ()
interactEval = interact . eval

batchEval :: Source -> Output
batchEval source = eval source ([]::String)

batchEvalIL :: Memory -> Output
batchEvalIL memory = evalIL memory ([]::String)

----

eval :: Source -> Interact
eval = evalIL . tokenize

evalIL :: Memory -> Interact
evalIL = doInstruction 0

doInstruction :: Symbol -> Memory -> Interact
doInstruction ic m
  | ic < 0    = doEnd
  | src < 0   = doInput  dst ic m
  | dst < 0   = doOutput src ic m
  | otherwise = doInstruction ic' $ storeToHalfTape dst value m
    where
      src = loadFromHalfTape m ic
      dst = loadFromHalfTape m $ ic + 1
      value = loadFromHalfTape m dst - loadFromHalfTape m src
      ic'
        | value <= 0 = loadFromHalfTape m $ ic + 2
        | otherwise  = ic + 3

doEnd :: Interact
doEnd _ = []

doInput :: Symbol -> Symbol -> Memory -> Interact
doInput _       _  _ []    = error "Empty input"
doInput address ic m (value:input) = doInstruction (ic+3) ( storeToHalfTape address (ord value) m) input

doOutput :: Symbol -> Symbol -> Memory -> Interact
doOutput address ic memory input = chr (loadFromHalfTape memory address) : doInstruction (ic+3) memory input
