module HelVM.HelMA.Automata.FALSE.Expression where

import           HelVM.HelMA.Automaton.Instruction

type ExpressionList = [Expression]
data Expression =
    Inst Instruction
  | Lambda ExpressionList
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
