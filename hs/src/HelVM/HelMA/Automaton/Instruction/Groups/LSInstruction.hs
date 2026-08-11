module HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction

-- | Types

data LSInstruction
  = Load --Restore --Fetch
  | LoadD !ImmediateIndex
  | Store --Save
  | StoreID !Integer !ImmediateIndex
  | MoveD !ImmediateIndex !ImmediateIndex
  | MIO !IOInstruction
  deriving stock (Eq, Read, Show)
