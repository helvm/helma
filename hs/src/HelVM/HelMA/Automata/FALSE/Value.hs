module HelVM.HelMA.Automata.FALSE.Value where

import           HelVM.HelMA.Automaton.Instruction

type ValueList = [Value]
data Value =
    Inst Instruction
  | Lambda ValueList
  | Exec
  | Cond
  | While
  | Ref Natural
  | Store
  | Fetch
  | Str String
  | Comment String
  | Flush
  deriving stock (Eq , Show , Read)
