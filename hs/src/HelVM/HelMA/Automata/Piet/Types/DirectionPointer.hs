module HelVM.HelMA.Automata.Piet.Types.DirectionPointer
  ( DirectionPointer (..)
  , addCoordinates
  , rotate
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Extra

addCoordinates ∷ DirectionPointer → Coordinates → Coordinates
addCoordinates DPRight (x, y) = (x + 1, y)
addCoordinates DPDown  (x, y) = (x, y + 1)
addCoordinates DPLeft  (x, y) = (x - 1, y)
addCoordinates DPUp    (x, y) = (x, y - 1)

rotate ∷ Int → DirectionPointer → DirectionPointer
rotate = change 4

data DirectionPointer
  = DPRight
  | DPDown
  | DPLeft
  | DPUp
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
