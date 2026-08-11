module HelVM.HelMA.Automata.Piet.Types.Brightness
  ( Brightness (..)
  , brightnessChange
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Extra

brightnessChange ∷ Brightness → Brightness → Brightness
brightnessChange = changeDiff 3

data Brightness
  = Light
  | Normal
  | Dark
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
