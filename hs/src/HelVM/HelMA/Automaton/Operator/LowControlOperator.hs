module HelVM.HelMA.Automaton.Operator.LowControlOperator where

import           HelVM.HelIO.Collections.SList

data LowControlOperator =
    Mark !Label
  | Call !Label
  | Jump !Label
  | Branch !BranchTest !Label
  | Return
  deriving stock (Eq , Show , Read)

data BranchTest = EZ | Neg
   deriving stock (Eq , Show , Read)

type Label = SString
