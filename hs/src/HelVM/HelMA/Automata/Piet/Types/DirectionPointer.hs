module HelVM.HelMA.Automata.Piet.Types.DirectionPointer
  ( DirectionPointer (..)
  , move
  , nextPointer
  , rotate
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Extra

-- TYPES

data DirectionPointer
  = DPRight
  | DPDown
  | DPLeft
  | DPUp
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- FUNCTIONS

move ∷ DirectionPointer → Coordinates → Coordinates
move DPRight (x, y) = (x + 1, y)
move DPDown  (x, y) = (x, y + 1)
move DPUp    (x, y) = (x, y - 1)
move DPLeft  (x, y) = (x - 1, y)

nextPointer ∷ DirectionPointer → DirectionPointer
nextPointer DPLeft  = DPUp
nextPointer DPUp    = DPRight
nextPointer DPRight = DPDown
nextPointer DPDown  = DPLeft

rotate ∷ Int → DirectionPointer → DirectionPointer
rotate = change 4
