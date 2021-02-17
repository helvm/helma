module HelVM.HelCam.Machines.SubLeq.Evaluator.MonadicEvaluator (
  monadicEval,
  evalIL
) where

import HelVM.HelCam.Machines.SubLeq.Lexer
import HelVM.HelCam.Machines.SubLeq.Symbol

import HelVM.HelCam.Common.RAM.SeqRAM as RAM
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

type Memory = RAM Symbol

monadicEval :: Source -> IO ()
monadicEval = eval

----

eval :: Source -> IO ()
eval = evalIL . tokenize

evalIL :: WrapperIO m => SymbolList -> m ()
evalIL il = doInstruction 0 $ RAM.fromList il

doInstruction :: WrapperIO m => Symbol -> Memory -> m ()
doInstruction ic m
  | ic < 0    = doEnd
  | src < 0   = doInputChar  dst ic m
  | dst < 0   = doOutputChar src ic m
  | otherwise = doInstruction ic' $ store dst diff m
    where
      src  = load m ic
      dst  = load m $ ic + 1
      diff = load m dst - load m src
      ic'
        | diff <= 0 = load m $ ic + 2
        | otherwise = ic + 3

doEnd :: WrapperIO m => m ()
doEnd = pass

doInputChar :: WrapperIO m => Symbol -> Symbol -> Memory -> m ()
doInputChar address ic m = do
  value <- wGetInt
  doInstruction (ic+3) $ store address value m

doOutputChar :: WrapperIO m => Symbol -> Symbol -> Memory -> m ()
doOutputChar address ic memory = do
  wPutInt $ load memory address
  doInstruction (ic+3) memory
