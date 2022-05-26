module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  next
) where

import           HelVM.HelMA.Automata.WhiteSpace.Symbol

import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Units.ALU        as Stack
import           HelVM.HelMA.Automaton.Units.CPU        as CPU
import           HelVM.HelMA.Automaton.Units.LSU        as LSU
import           HelVM.HelMA.Automaton.Units.Unit

import           HelVM.HelIO.Containers.LLIndexSafe

import           Control.Type.Operator

import           Prelude                                hiding (swap)

next :: (SREvaluator Symbol s r m) => ControlUnit -> s -> r -> m $ Unit s r
next (CU il ic is) s r = doInstruction' =<< indexSafe il ic where doInstruction' i = doInstruction i (CU il (ic+1) is) s r

stackNext :: (SREvaluator Symbol s r m) => ControlUnit -> r -> s -> m (Unit s r)
stackNext cu r s = next cu s r

cuNext :: (SREvaluator Symbol s r m) => r -> ControlUnit -> s -> m (Unit s r)
cuNext r cu s = next cu s r

----

doInstruction :: (SREvaluator Symbol s r m) => Instruction -> ControlUnit -> s -> r -> m (Unit s r)
doInstruction (IAL      i) cu s r = stackNext cu r =<< alInstruction i s
doInstruction (ILS      i) cu s r = uncurry (next cu) . sluToTuple =<< slInstruction i (LSU s r)
doInstruction (IControl i) cu s r = uncurry (cuNext r). cpuToTuple =<< controlInstruction i (CPU cu s)
doInstruction  End         cu s r = pure $ Unit cu s r
