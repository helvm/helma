{-# Language ConstraintKinds   #-}
{-# Language FlexibleContexts  #-}
{-# Language FlexibleInstances #-}
module HelVM.HelCam.Machines.SubLeq.Evaluator (
  batchSimpleEval,
  batchSimpleEvalIL,
  simpleEvalIL,
  eval,
) where

import HelVM.HelCam.Machines.SubLeq.Lexer
import HelVM.HelCam.Machines.SubLeq.Symbol

import HelVM.HelCam.Common.RAM as RAM
import HelVM.HelCam.Common.Types.RAMType
import HelVM.HelCam.Common.Util
import HelVM.HelCam.Common.WrapperIO

batchSimpleEval :: Source -> Output
batchSimpleEval = flip simpleEval emptyInput

batchSimpleEvalIL :: SymbolList -> Output
batchSimpleEvalIL = flip simpleEvalIL emptyInput

simpleEval :: Evaluator r => Source -> r
simpleEval source = eval source defaultRAMType

simpleEvalIL :: Evaluator r => SymbolList -> r
simpleEvalIL il = evalIL il defaultRAMType

eval :: Evaluator r => Source -> RAMType -> r
eval source = evalIL $ tokenize source

evalIL :: Evaluator r => SymbolList -> RAMType -> r
evalIL il ListRAMType   = start (RAM.fromList il::SymbolList)
evalIL il SeqRAMType    = start (RAM.fromList il::Seq Symbol)
evalIL il IntMapRAMType = start (RAM.fromList il::IntMap Symbol)

start ::(RAM Symbol m, Evaluator r) => m -> r
start = doInstruction 0

class Evaluator r where
  doInstruction :: RAM Symbol m => Symbol -> m -> r
  doInstruction ic memory
    | ic  < 0   = doEnd
    | src < 0   = doInputChar  dst ic memory
    | dst < 0   = doOutputChar src ic memory
    | otherwise = doInstruction ic' $ store dst diff memory
      where
        src  = load memory ic
        dst  = load memory $ ic + 1
        diff = load memory dst - load memory src :: Symbol
        ic'
          | diff <= 0 = (load memory $ ic + 2) :: Symbol
          | otherwise = ic + 3

  doEnd        :: r
  doInputChar  :: RAM Symbol m => Symbol -> Symbol -> m -> r
  doOutputChar :: RAM Symbol m => Symbol -> Symbol -> m -> r

----

instance Evaluator Interact where
  doEnd _ = []

  doInputChar _       _  _ []    = error "Empty input"
  doInputChar address ic memory (value:input) = doInstruction (ic+3) (store address (ord value) memory) input

  doOutputChar address ic memory input = chr (load memory address) : doInstruction (ic+3) memory input

----

instance WrapperIO m => Evaluator (m ()) where
  doEnd = pass

  doInputChar address ic memory = do
    value <- wGetInt
    doInstruction (ic+3) $ store address value memory

  doOutputChar address ic memory = do
    wPutInt (load memory address :: Symbol)
    doInstruction (ic+3) memory
