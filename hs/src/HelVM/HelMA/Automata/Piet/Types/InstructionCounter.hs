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

import           Lens.Micro                                       ( (%~), (^.) )
import           Lens.Micro.TH                                    ( makeLenses )

data InstructionCounter
  = InstructionCounter
      { _position    :: !Coordinates
      , _orientation :: !Orientation
      }

makeLenses ''InstructionCounter

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
