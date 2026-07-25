module HelVM.HelMA.Automata.Piet.Types.InstructionCounter (
  directionPointerIC,
  codelChooserIC,
  rotateDirectionPointerIC,
  toggleCodelChooserIC,
  initialInstructionCounter,
  InstructionCounter(..),
) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Orientation

directionPointerIC :: InstructionCounter -> DirectionPointer
directionPointerIC = directionPointer .  orientation

codelChooserIC :: InstructionCounter -> CodelChooser
codelChooserIC = codelChooser . orientation

rotateDirectionPointerIC :: Int -> InstructionCounter -> InstructionCounter
rotateDirectionPointerIC n ic = ic { orientation = rotateDirectionPointer n (orientation ic)}

toggleCodelChooserIC :: Int -> InstructionCounter -> InstructionCounter
toggleCodelChooserIC n ic = ic { orientation = toggleCodelChooser n (orientation ic)}

initialInstructionCounter :: InstructionCounter
initialInstructionCounter = InstructionCounter initialCoordinates initialOrientation

data InstructionCounter = InstructionCounter
  { position    :: !Coordinates
  , orientation :: !Orientation
  }
