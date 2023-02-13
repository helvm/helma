module HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction where

import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction

-- | Types

data LSInstruction =
    Load  --Restore --Fetch
  | Store --Save
  | MIO   !IOInstruction
  deriving stock (Eq , Read , Show)
