module HelVM.HelMA.Automaton.Automaton (
  startWithIL,
  start,
) where

import           HelVM.HelMA.Automaton.Evaluator
import           HelVM.HelMA.Automaton.IO.EvaluatorIO
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Symbol
import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Units.CPU

import qualified Data.Vector                          as Vector

startWithIL :: (SREvaluator Symbol s r m) => s -> r -> DumpType -> InstructionList -> m ()
startWithIL s r dt il = start il s r dt

start :: (SREvaluator Symbol s r m) => InstructionList -> s -> r -> DumpType -> m ()
start il s r dt = logDump dt =<< next (CU (Vector.fromList il) 0 (IS [])) s r
