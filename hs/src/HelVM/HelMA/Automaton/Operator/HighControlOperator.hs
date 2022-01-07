module HelVM.HelMA.Automaton.Operator.HighControlOperator where

data HighControlOperator =
    Apply
  | When
  | While
  deriving stock (Eq , Show , Read)
