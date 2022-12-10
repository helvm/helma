module HelVM.HelMA.Automaton.Automaton (
  startWithIL,
  start,
) where

import           HelVM.HelMA.Automaton.Evaluator
import           HelVM.HelMA.Automaton.IO.EvaluatorIO
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Symbol
import           HelVM.HelMA.Automaton.Types.DumpType

import           HelVM.HelMA.Automaton.Units.Unit

startWithIL :: (SREvaluator Symbol s r m) => s -> r -> Maybe Natural -> DumpType -> InstructionList -> m ()
startWithIL s r limit dt il = start il s r limit dt

start :: (SREvaluator Symbol s r m) => InstructionList -> s -> r -> Maybe Natural -> DumpType -> m ()
start il s r limit dt = logDump dt =<< eval limit (newUnit il s r)
