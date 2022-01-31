module HelVM.HelMA.Automaton.Units.Unit where

import           HelVM.HelMA.Automaton.Units.CPU

-- | Data types
data Unit s r = Unit
  { unitCU    :: ControlUnit
  , unitStack :: s
  , unitRAM   :: r
  }
  deriving stock (Show)
