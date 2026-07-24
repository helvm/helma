module HelVM.HelMA.Automata.Piet.Types.InstructionCounter (
  initialInstructionCounter,
  InstructionCounter(..),
) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Orientation

initialInstructionCounter :: InstructionCounter
initialInstructionCounter = InstructionCounter initialCoordinates initialOrientation

data InstructionCounter = InstructionCounter
  { position    :: !Coordinates
  , orientation :: !Orientation
  }
