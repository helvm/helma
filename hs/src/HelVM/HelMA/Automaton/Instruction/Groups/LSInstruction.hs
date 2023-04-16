module HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction

-- | Types

data LSInstruction =
    Load    --Restore --Fetch
  | LoadD   !Index
  | Store   --Save
  | StoreID !Integer !Index
  | MoveD   !Index !Index
  | MIO     !IOInstruction
  deriving stock (Eq , Read , Show)
