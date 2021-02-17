{-# Language FlexibleInstances #-}
module HelVM.HelCam.Machines.SubLeq.Evaluator (
  interactEval,
  batchEval,
  monadicEval,
  batchEvalIL,
  evalIL,
) where

import HelVM.HelCam.Machines.SubLeq.Lexer
import HelVM.HelCam.Machines.SubLeq.Symbol

import HelVM.HelCam.Common.RAM.IntMapRAM as RAM
import HelVM.HelCam.Common.MockIO
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

type Memory = RAM Symbol

class Evaluator r where
  eval :: Source -> r
  eval = evalIL . tokenize

  evalIL :: SymbolList -> r
  evalIL il = doInstruction 0 $ RAM.fromList il

  doInstruction :: Symbol -> Memory -> r
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

  doEnd    :: r
  doInputChar  :: Symbol -> Symbol -> Memory -> r
  doOutputChar :: Symbol -> Symbol -> Memory -> r

----

interactEval :: Source -> IO ()
interactEval _ = pass

batchEval :: Source -> Output
batchEval source = eval source ([]::String)

batchEvalIL :: SymbolList -> Output
batchEvalIL memory = evalIL memory ([]::String)

instance Evaluator (Input -> Output) where
  doEnd _ = []

  doInputChar _       _  _ []    = error "Empty input"
  doInputChar address ic m (value:input) = doInstruction (ic+3) (store address (ord value) m) input

  doOutputChar address ic memory input = chr (load memory address) : doInstruction (ic+3) memory input

----

monadicEval :: Source -> IO ()
monadicEval = eval

instance Evaluator (IO ()) where
  doEnd = pass

  doInputChar address ic m = do
    value <- wGetInt
    doInstruction (ic+3) $ store address value m

  doOutputChar address ic memory = do
    wPutInt $ load memory address
    doInstruction (ic+3) memory

----

instance Evaluator (MockIO ()) where
  doEnd = pass

  doInputChar address ic m = do
    value <- mockGetInt
    doInstruction (ic+3) $ store address value m

  doOutputChar address ic memory = do
    mockPutInt $ load memory address
    doInstruction (ic+3) memory
