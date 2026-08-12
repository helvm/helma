module HelVM.HelMA.Automata.Piet.Types.Lightness
  ( Lightness (..)
  , brightnessChange
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Extra

brightnessChange ∷ Lightness → Lightness → Lightness
brightnessChange = changeDiff 3

data Lightness
  = Light
  | Normal
  | Dark
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
