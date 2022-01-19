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
import           HelVM.HelMA.Automaton.Memories.LLRAM as RAM
import           HelVM.HelMA.Automaton.Types.RAMType

import qualified HelVM.Common.Collections.MapList     as MapList
import qualified HelVM.Common.Collections.SList       as SList

import           HelVM.Common.Control.Logger

import qualified Data.Sequence                        as Seq

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
doInstruction ic ram
  | ic  < 0   = doEnd ic ram
  | src < 0   = doInputChar  dst ic ram
  | dst < 0   = doOutputChar src ic ram
  | otherwise = doInstruction ic' $ store dst diff ram
    where
      src  = genericLoad ram ic
      dst  = genericLoad ram $ ic + 1
      diff = genericLoad ram dst - genericLoad ram src
      ic'
        | diff <= 0 = genericLoad ram $ ic + 2
        | otherwise = ic + 3

-- | IO instructions
doOutputChar :: RLLEvaluator e r m => e -> e -> r -> m ()
doOutputChar address ic ram = wPutIntegral (genericLoad ram address) *> doInstruction (ic+3) ram

doInputChar :: RLLEvaluator e r m => e -> e -> r -> m ()
doInputChar address ic ram = doInputChar' =<< wGetChar where
  doInputChar' char = doInstruction (ic+3) $ storeChar address char ram

-- | Terminate instruction
doEnd :: RLLEvaluator e r m => e -> r -> m ()
doEnd ic ram = logMessageTuple ("ic" , show ic) *> logMessageTuple ("ram" , show ram)
