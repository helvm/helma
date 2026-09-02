module HelVM.HelMA.Automata.Piet.Types.DirectionPointer
  ( DirectionPointer (..)
  , charDP
  , move
  , nextPointer
  , rotate
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Cyclic

import           Relude.Extra

-- TYPES

data DirectionPointer
  = DPRight
  | DPDown
  | DPLeft
  | DPUp
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- FUNCTIONS

move ∷ DirectionPointer → Coordinates → Coordinates
move DPRight = first  next
move DPDown  = second next
move DPLeft  = first  prev
move DPUp    = second prev

nextPointer ∷ DirectionPointer → DirectionPointer
nextPointer DPRight = DPDown
nextPointer DPDown  = DPLeft
nextPointer DPLeft  = DPUp
nextPointer DPUp    = DPRight

rotate ∷ Int → DirectionPointer → DirectionPointer
rotate = cyclicMove

charDP ∷ DirectionPointer → Char
charDP DPRight = 'r'
charDP DPDown  = 'd'
charDP DPLeft  = 'l'
charDP DPUp    = 'u'
