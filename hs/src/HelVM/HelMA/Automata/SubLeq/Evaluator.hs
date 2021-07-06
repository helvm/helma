module HelVM.HelMA.Automata.SubLeq.Evaluator (
  simpleEval,
  simpleEvalIL,
  evalParams,
  eval,
) where

import HelVM.HelMA.Automata.SubLeq.Lexer
import HelVM.HelMA.Automata.SubLeq.Symbol

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.API.TypeOptions
import HelVM.Common.Containers.FromList
import HelVM.HelMA.Automaton.IO.BusinessIO
import HelVM.HelMA.Automaton.Memories.RAMConst as RAM
import HelVM.HelMA.Automaton.Types.RAMType

import qualified Data.Sequence as Seq

simpleEval :: Evaluator Symbol m => Source -> m ()
simpleEval source = eval source defaultRAMType

simpleEvalIL :: Evaluator Symbol m => SymbolList -> m ()
simpleEvalIL il = evalIL il defaultRAMType

evalParams :: (Evaluator Symbol m) => EvalParams -> m ()
evalParams p = eval (source p) (ram $ typeOptions p)

eval :: Evaluator Symbol m => Source -> RAMType -> m ()
eval source = evalIL $ tokenize source

evalIL :: Evaluator e m => [e] -> RAMType -> m ()
evalIL = flip evalIL'

evalIL' :: Evaluator e m => RAMType -> [e] -> m ()
evalIL' ListRAMType   = start
evalIL' SeqRAMType    = start . Seq.fromList
evalIL' IntMapRAMType = start . intMapFromList

start :: REvaluator e r m => r -> m ()
start = doInstruction 0

doInstruction :: REvaluator e r m => e -> r -> m ()
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

-- IO instructions
doOutputChar :: REvaluator e r m => e -> e -> r -> m ()
doOutputChar address ic memory = wPutIntegral (genericLoad memory address) *> doInstruction (ic+3) memory

doInputChar :: REvaluator e r m=> e -> e -> r -> m ()
doInputChar address ic memory = doInputChar' =<< wGetChar where
  doInputChar' char = doInstruction (ic+3) $ storeChar address char memory

-- Terminate instruction
doEnd :: REvaluator e r m => e -> r -> m ()
doEnd ic _ = wLogStrLn (show ic)
