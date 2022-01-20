module HelVM.HelMA.Automaton.Instruction.LSInstruction where

import           HelVM.HelMA.Automaton.Instruction.IOInstruction

-- | Types

data LSInstruction =
    Load  --Restore --Fetch
  | Store --Save
  | MIO   !IOInstruction
  deriving stock (Eq , Show , Read)
