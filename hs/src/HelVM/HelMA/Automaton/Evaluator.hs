module HelVM.HelMA.Automaton.Evaluator (
  startWithIL,
  start,
  startAutomaton,
) where

import           HelVM.HelMA.Automaton.Automaton
import           HelVM.HelMA.Automaton.IO.AutomatonIO
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Symbol
import           HelVM.HelMA.Automaton.Types.DumpType

startWithIL :: (SRAutomatonIO Symbol s r m) => s -> r -> Maybe Natural -> DumpType -> InstructionList -> m ()
startWithIL s r limit dt il = start il s r limit dt

start :: (SRAutomatonIO Symbol s r m) => InstructionList -> s -> r -> Maybe Natural -> DumpType -> m ()
start il s r limit dt = startAutomaton dt limit (newAutomaton il s r)

startAutomaton :: (SRAutomatonIO Symbol s r m) => DumpType -> Maybe Natural -> Automaton s r ->  m ()
startAutomaton dt limit = logDump dt <=< run limit
