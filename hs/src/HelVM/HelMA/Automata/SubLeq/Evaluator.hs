module HelVM.HelMA.Automata.SubLeq.Evaluator (
  batchSimpleEval,
  batchSimpleEvalIL,
  flipSimpleEval,
  flipSimpleEvalIL,
  simpleEval,
  simpleEvalIL,
  evalParams,
  eval,
) where

import HelVM.HelMA.Automata.SubLeq.Lexer
import HelVM.HelMA.Automata.SubLeq.Symbol

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.API.TypeOptions
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Memories.RAM as RAM
import HelVM.HelMA.Common.Types.RAMType
import HelVM.HelMA.Common.Util

batchSimpleEval :: Source -> Output
batchSimpleEval = flipSimpleEval emptyInput

batchSimpleEvalIL :: SymbolList -> Output
batchSimpleEvalIL = flipSimpleEvalIL emptyInput

flipSimpleEval :: Input -> Source -> Output
flipSimpleEval = flip simpleEval

flipSimpleEvalIL :: Input -> SymbolList -> Output
flipSimpleEvalIL = flip simpleEvalIL

simpleEval :: Evaluator r => Source -> r
simpleEval source = eval source defaultRAMType

simpleEvalIL :: Evaluator r => SymbolList -> r
simpleEvalIL il = evalIL il defaultRAMType

evalParams :: Evaluator r => EvalParams ->  r
evalParams p = eval (source p) (ram $ typeOptions p)

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

  doInputChar address ic memory = doInputChar' =<< wGetInt where
    doInputChar' value = doInstruction (ic+3) $ store address value memory

  doOutputChar address ic memory = wPutInt (load memory address :: Symbol) *> doInstruction (ic+3) memory
