module HelVM.HelMA.Automata.SubLeq.Evaluator (
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
import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Units.RAM      as RAM

import qualified HelVM.Common.Collections.MapList     as MapList
import qualified HelVM.Common.Collections.SList       as SList

import           HelVM.Common.Control.Logger

import qualified Data.Sequence                        as Seq

flippedEval :: Evaluator Symbol m => RAMType -> Source -> m ()
flippedEval = flip eval

flippedEvalIL :: Evaluator Symbol m => RAMType -> SymbolList -> m ()
flippedEvalIL = flip evalIL

evalParams :: (Evaluator Symbol m) => EvalParams -> m ()
evalParams p = eval (source p) (ramTypeOptions p)

eval :: Evaluator Symbol m => Source -> RAMType -> m ()
eval source = evalIL $ tokenize source

evalIL :: Evaluator e m => [e] -> RAMType -> m ()
evalIL = flip evalIL'

evalIL' :: Evaluator e m => RAMType -> [e] -> m ()
evalIL' ListRAMType    = start
evalIL' SeqRAMType     = start . Seq.fromList
evalIL' SListRAMType   = start . SList.sListFromList
evalIL' MapListRAMType = start . MapList.mapListFromList

start :: REvaluator e r m => r -> m ()
start = doInstruction 0

doInstruction :: REvaluator e r m => e -> r -> m ()
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
doOutputChar :: REvaluator e r m => e -> e -> r -> m ()
doOutputChar address ic ram = wPutAsChar (genericLoad ram address) *> doInstruction (ic+3) ram

doInputChar :: REvaluator e r m => e -> e -> r -> m ()
doInputChar address ic ram = doInputChar' =<< wGetChar where
  doInputChar' char = doInstruction (ic+3) $ storeChar address char ram

-- | Terminate instruction
doEnd :: REvaluator e r m => e -> r -> m ()
doEnd ic ram = logMessageTuple ("ic" , show ic) *> logMessageTuple ("ram" , show ram)
