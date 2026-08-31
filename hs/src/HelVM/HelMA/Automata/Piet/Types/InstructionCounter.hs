module HelVM.HelMA.Automata.Piet.Types.InstructionCounter
  ( InstructionCounter (..)
  , codelChooserIC
  , directionPointerIC
  , initialInstructionCounter
  , orientation
  , position
  , rotateDirectionPointerIC
  , toggleCodelChooserIC
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Orientation
  ( Orientation
  , codelChooser
  , directionPointer
  , initialOrientation
  , rotateDirectionPointer
  , toggleCodelChooser
  )

import           Relude.Extra

-- TYPES & LENSES

data InstructionCounter
  = InstructionCounter
      { _position    :: !Coordinates
      , _orientation :: !Orientation
      }
  deriving stock (Eq, Show)

position ∷ Lens' InstructionCounter Coordinates
position = lens _position (\s x -> s { _position = x })

orientation ∷ Lens' InstructionCounter Orientation
orientation = lens _orientation (\s x -> s { _orientation = x })

-- HELPER FUNCTIONS

directionPointerIC ∷ InstructionCounter → DirectionPointer
directionPointerIC ic = ic ^. (orientation . directionPointer)

codelChooserIC ∷ InstructionCounter → CodelChooser
codelChooserIC ic = ic ^. (orientation . codelChooser)

rotateDirectionPointerIC ∷ Int → InstructionCounter → InstructionCounter
rotateDirectionPointerIC n = orientation %~ rotateDirectionPointer n

toggleCodelChooserIC ∷ Int → InstructionCounter → InstructionCounter
toggleCodelChooserIC n = orientation %~ toggleCodelChooser n

initialInstructionCounter ∷ InstructionCounter
initialInstructionCounter = InstructionCounter initialCoordinates initialOrientation
