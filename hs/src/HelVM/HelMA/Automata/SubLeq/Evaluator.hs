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
import HelVM.HelMA.Common.Collections.FromList
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Memories.RAMConst as RAM
import HelVM.HelMA.Common.Types.RAMType
import HelVM.HelMA.Common.Util

import Data.Default as Default

import qualified Data.Sequence as Seq

batchSimpleEval :: Source -> Output
batchSimpleEval = flipSimpleEval emptyInput

batchSimpleEvalIL :: SymbolList -> Output
batchSimpleEvalIL = flipSimpleEvalIL emptyInput

flipSimpleEval :: Input -> Source -> Output
flipSimpleEval = flip simpleEval

flipSimpleEvalIL :: Input -> SymbolList -> Output
flipSimpleEvalIL = flip simpleEvalIL

simpleEval :: Evaluator Symbol r => Source -> r
simpleEval source = eval source defaultRAMType

simpleEvalIL :: Evaluator Symbol r => SymbolList -> r
simpleEvalIL il = evalIL il defaultRAMType

evalParams :: Evaluator Symbol r => EvalParams -> r
evalParams p = eval (source p) (ram $ typeOptions p)

eval :: Evaluator Symbol r => Source -> RAMType -> r
eval source = evalIL $ tokenize source

class (Default cell , Integral cell) => Evaluator cell r where

  evalIL :: [cell] -> RAMType -> r
  evalIL = flip evalIL'

  evalIL' :: RAMType -> [cell] -> r
  evalIL' ListRAMType   = start
  evalIL' SeqRAMType    = start . Seq.fromList
  evalIL' IntMapRAMType = start . intMapFromList

  start :: RAM cell m => m -> r
  start = doInstruction 0

  doInstruction :: RAM cell m => cell -> m -> r
  doInstruction ic memory
    | ic  < 0   = doEnd ic memory
    | src < 0   = doInputChar  dst ic memory
    | dst < 0   = doOutputChar src ic memory
    | otherwise = doInstruction ic' $ store dst diff memory
      where
        src  = genericLoad memory ic
        dst  = genericLoad memory $ ic + 1
        diff = genericLoad memory dst - genericLoad memory src
        ic'
          | diff <= 0 = genericLoad memory $ ic + 2
          | otherwise = ic + 3

  doEnd        :: RAM cell m => cell -> m -> r
  doInputChar  :: RAM cell m => cell -> cell -> m -> r
  doOutputChar :: RAM cell m => cell -> cell -> m -> r

----

instance (Default cell , Integral cell) => Evaluator cell Interact where
  doEnd _ _ _ = []

  doInputChar _       _  _ []    = error "Empty input"
  doInputChar address ic memory (char:input) = doInstruction (ic+3) (storeChar address char memory) input

  doOutputChar address ic memory input = genericChr (genericLoad memory address) : doInstruction (ic+3) memory input

----

instance (Show cell , Default cell , Integral cell , WrapperIO m) => Evaluator cell (m ()) where
  doEnd ic _ = wLogStrLn (show ic)

  doInputChar address ic memory = doInputChar' =<< wGetChar where
    doInputChar' char = doInstruction (ic+3) $ storeChar address char memory

  doOutputChar address ic memory = wPutIntegral (genericLoad memory address) *> doInstruction (ic+3) memory
