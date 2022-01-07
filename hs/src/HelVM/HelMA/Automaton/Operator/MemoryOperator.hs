module HelVM.HelMA.Automaton.Operator.MemoryOperator where

import           HelVM.HelMA.Automaton.Operator.IOOperator

-- | Types

data MemoryOperator =
    Store --Save
  | Load  --Restore --Fetch
  | IOOp IOOperator
  deriving stock (Eq , Show , Read)
