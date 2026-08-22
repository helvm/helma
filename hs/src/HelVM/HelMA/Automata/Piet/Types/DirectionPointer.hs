module HelVM.HelMA.Automata.Piet.Types.DirectionPointer
  ( DirectionPointer (..)
  , addCoordinates
  , move
  , nextPointer
  , rotate
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Extra

import           Lens.Micro

addCoordinates ∷ DirectionPointer → Coordinates → Coordinates
addCoordinates DPRight (x, y) = (x + 1, y)
addCoordinates DPDown  (x, y) = (x, y + 1)
addCoordinates DPLeft  (x, y) = (x - 1, y)
addCoordinates DPUp    (x, y) = (x, y - 1)

move ∷ DirectionPointer → Coordinates → Coordinates
move DPLeft  = _1 -~ 1
move DPRight = _1 +~ 1
move DPUp    = _2 -~ 1
move DPDown  = _2 +~ 1

nextPointer ∷ DirectionPointer → DirectionPointer
nextPointer DPLeft  = DPUp
nextPointer DPUp    = DPRight
nextPointer DPRight = DPDown
nextPointer DPDown  = DPLeft

rotate ∷ Int → DirectionPointer → DirectionPointer
rotate = change 4

data DirectionPointer
  = DPRight
  | DPDown
  | DPLeft
  | DPUp
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
