{-# Language FlexibleInstances #-}
module HelVM.HelCam.Machines.SubLeq.Evaluator where

import HelVM.HelCam.Machines.SubLeq.EvaluatorUtil
import HelVM.HelCam.Machines.SubLeq.Lexer

import HelVM.HelCam.Common.MockIO
import HelVM.HelCam.Common.Tape
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

class Evaluator r where
  eval :: Source -> r
  eval = evalIL . tokenize

  evalIL :: Memory -> r
  evalIL il = doInstruction 0 il

  doInstruction :: Symbol -> Memory -> r
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

  doEnd    :: r
  doInput  :: Symbol -> Symbol -> Memory -> r
  doOutput :: Symbol -> Symbol -> Memory -> r

----

interactEval :: Source -> IO ()
interactEval _ = pass

batchEval :: Source -> Output
batchEval source = eval source ([]::String)

batchEvalIL :: Memory -> Output
batchEvalIL memory = evalIL memory ([]::String)

instance Evaluator (Input -> Output) where
  doEnd _ = []

  doInput _       _  _ []    = error "Empty input"
  doInput address ic m (value:input) = doInstruction (ic+3) ( storeToHalfTape address (ord value) m) input

  doOutput address ic memory input = chr (loadFromHalfTape memory address) : doInstruction (ic+3) memory input

----

monadicEval :: Source -> IO ()
monadicEval = eval

instance Evaluator (IO ()) where
  doEnd = pass

  doInput address ic m = do
    value <- wGetInt
    doInstruction (ic+3) $ storeToHalfTape address value m

  doOutput address ic memory = do
    wPutInt $ loadFromHalfTape memory address
    doInstruction (ic+3) memory

----

instance Evaluator (MockIO ()) where
  doEnd = pass

  doInput address ic m = do
    value <- mockGetInt
    doInstruction (ic+3) $ storeToHalfTape address value m

  doOutput address ic memory = do
    mockPutInt $ loadFromHalfTape memory address
    doInstruction (ic+3) memory
