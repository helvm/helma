module HelVM.HelCam.Machines.SubLeq.Evaluator.MonadicEvaluator where

import HelVM.HelCam.Machines.SubLeq.EvaluatorUtil
import HelVM.HelCam.Machines.SubLeq.Lexer

import HelVM.HelCam.Common.Tape
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

monadicEval :: Source -> IO ()
monadicEval = eval

----

eval :: Source -> IO ()
eval = evalIL . tokenize

evalIL :: WrapperIO m => Memory -> m ()
evalIL = doInstruction 0

doInstruction :: WrapperIO m => Symbol -> Memory -> m ()
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

doEnd :: WrapperIO m => m ()
doEnd = return ()

doInput :: WrapperIO m => Symbol -> Symbol -> Memory -> m ()
doInput address ic m = do
  value <- wGetInt
  doInstruction (ic+3) $ storeToHalfTape address value m

doOutput :: WrapperIO m => Symbol -> Symbol -> Memory -> m ()
doOutput address ic memory = do
  wPutInt $ loadFromHalfTape memory address
  doInstruction (ic+3) memory
