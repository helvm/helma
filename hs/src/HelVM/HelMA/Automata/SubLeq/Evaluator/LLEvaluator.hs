module HelVM.HelMA.Automata.SubLeq.Evaluator.LLEvaluator (
  flippedEval,
  flippedEvalIL,
  evalParams,
  eval,
) where

import           HelVM.HelMA.Automata.SubLeq.Lexer
import           HelVM.HelMA.Automata.SubLeq.Symbol

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.Memories.RAM.LLRAM as RAM
import           HelVM.HelMA.Automaton.Types.RAMType

import qualified HelVM.HelIO.Collections.MapList          as MapList
import qualified HelVM.HelIO.Collections.SList            as SList

import           HelVM.HelIO.Control.Logger

import qualified Data.Sequence                            as Seq

flippedEval :: LLEvaluator Symbol m => RAMType -> Source -> m ()
flippedEval = flip eval

flippedEvalIL :: LLEvaluator Symbol m => RAMType -> SymbolList -> m ()
flippedEvalIL = flip evalIL

evalParams :: (LLEvaluator Symbol m) => EvalParams -> m ()
evalParams p = eval (source p) (ramTypeOptions p)

eval :: LLEvaluator Symbol m => Source -> RAMType -> m ()
eval source = evalIL $ tokenize source

evalIL :: LLEvaluator e m => [e] -> RAMType -> m ()
evalIL = flip evalIL'

evalIL' :: LLEvaluator e m => RAMType -> [e] -> m ()
evalIL' ListRAMType    = start
evalIL' SeqRAMType     = start . Seq.fromList
evalIL' SListRAMType   = start . SList.sListFromList
evalIL' MapListRAMType = start . MapList.mapListFromList

start :: RLLEvaluator e r m => r -> m ()
start = doInstruction 0

doInstruction :: RLLEvaluator e r m => e -> r -> m ()
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

-- | IO instructions
doOutputChar :: RLLEvaluator e r m => e -> e -> r -> m ()
doOutputChar address ic memory = wPutAsChar (genericLoad memory address) *> doInstruction (ic+3) memory

doInputChar :: RLLEvaluator e r m=> e -> e -> r -> m ()
doInputChar address ic memory = doInputChar' =<< wGetChar where
  doInputChar' char = doInstruction (ic+3) $ storeChar address char memory

-- | Terminate instruction
doEnd :: RLLEvaluator e r m => e -> r -> m ()
doEnd ic _ = logData ic
