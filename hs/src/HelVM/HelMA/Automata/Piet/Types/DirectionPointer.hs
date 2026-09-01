module HelVM.HelMA.Automata.Piet.Types.DirectionPointer
  ( DirectionPointer (..)
  , charDP
  , move
  , nextPointer
  , rotate
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Cyclic

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
move DPLeft  (x, y) = (x - 1, y)
move DPUp    (x, y) = (x, y - 1)

nextPointer ∷ DirectionPointer → DirectionPointer
nextPointer DPRight = DPDown
nextPointer DPDown  = DPLeft
nextPointer DPLeft  = DPUp
nextPointer DPUp    = DPRight

rotate ∷ Int → DirectionPointer → DirectionPointer
rotate = cyclicMove 4

charDP ∷ DirectionPointer → Char
charDP DPRight = 'r'
charDP DPDown  = 'd'
charDP DPLeft  = 'l'
charDP DPUp    = 'u'
