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
import HelVM.HelMA.Automaton.IO.WrapperIO
import HelVM.HelMA.Automaton.Memories.RAMConst as RAM
import HelVM.HelMA.Automaton.Types.RAMType

import HelVM.Common.SafeMonadT

import Data.Default as Default

import qualified Data.Sequence as Seq

simpleEval :: Evaluator Symbol m_ => Source -> m_
simpleEval source = eval source defaultRAMType

simpleEvalIL :: Evaluator Symbol m_ => SymbolList -> m_
simpleEvalIL il = evalIL il defaultRAMType

evalParams :: (Monad m , Evaluator Symbol (m ())) => EvalParams -> SafeMonadT_ m
evalParams p = hoistMonad $ eval (source p) (ram $ typeOptions p)

eval :: Evaluator Symbol m_ => Source -> RAMType -> m_
eval source = evalIL $ tokenize source

class (Default e , Integral e) => Evaluator e m_ where

  evalIL :: [e] -> RAMType -> m_
  evalIL = flip evalIL'

  evalIL' :: RAMType -> [e] -> m_
  evalIL' ListRAMType   = start
  evalIL' SeqRAMType    = start . Seq.fromList
  evalIL' IntMapRAMType = start . intMapFromList

  start :: RAM e r => r -> m_
  start = doInstruction 0

  doInstruction :: RAM e r => e -> r -> m_
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

  doEnd        :: RAM e r => e -> r -> m_
  doInputChar  :: RAM e r => e -> e -> r -> m_
  doOutputChar :: RAM e r => e -> e -> r -> m_

----

instance (Show e , Default e , Integral e , WrapperIO m) => Evaluator e (m ()) where
  doEnd ic _ = wLogStrLn (show ic)

  doInputChar address ic memory = doInputChar' =<< wGetChar where
    doInputChar' char = doInstruction (ic+3) $ storeChar address char memory

  doOutputChar address ic memory = wPutIntegral (genericLoad memory address) *> doInstruction (ic+3) memory
