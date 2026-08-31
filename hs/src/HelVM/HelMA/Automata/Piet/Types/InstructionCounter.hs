module HelVM.HelMA.Automata.Piet.Types.InstructionCounter
  ( InstructionCounter (..)
  , codelChooserIC
  , directionPointerIC
  , initialInstructionCounter
  , orientationL
  , positionL
  , rotateDirectionPointerIC
  , toggleCodelChooserIC
  ) where

import           HelVM.HelMA.Automata.Piet.Types.CodelChooser
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.DirectionPointer
import           HelVM.HelMA.Automata.Piet.Types.Orientation
  ( Orientation
  , codelChooserL
  , directionPointerL
  , initialOrientation
  , rotateDirectionPointer
  , toggleCodelChooser
  )

import           Relude.Extra

-- TYPES & LENSES

data InstructionCounter
  = InstructionCounter
      { position    :: !Coordinates
      , orientation :: !Orientation
      }
  deriving stock (Eq, Show)

positionL ∷ Lens' InstructionCounter Coordinates
positionL = lens position (\s x -> s { position = x })

orientationL ∷ Lens' InstructionCounter Orientation
orientationL = lens orientation (\s x -> s { orientation = x })

-- HELPER FUNCTIONS

directionPointerIC ∷ InstructionCounter → DirectionPointer
directionPointerIC ic = ic ^. (orientationL . directionPointerL)

codelChooserIC ∷ InstructionCounter → CodelChooser
codelChooserIC ic = ic ^. (orientationL . codelChooserL)

rotateDirectionPointerIC ∷ Int → InstructionCounter → InstructionCounter
rotateDirectionPointerIC n = orientationL %~ rotateDirectionPointer n

toggleCodelChooserIC ∷ Int → InstructionCounter → InstructionCounter
toggleCodelChooserIC n = orientationL %~ toggleCodelChooser n

initialInstructionCounter ∷ InstructionCounter
initialInstructionCounter = InstructionCounter initialCoordinates initialOrientation
