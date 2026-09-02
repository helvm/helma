module HelVM.HelMA.Automata.Piet.Types.Hue
  ( Hue (..)
  , hueChange
  , hueDiff
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Cyclic

hueDiff ∷ Hue → Hue → Int
hueDiff = cyclicDiff

hueChange ∷ Hue → Hue → Hue
hueChange = cyclicChange

data Hue
  = Red
  | Yellow
  | Green
  | Cyan
  | Blue
  | Magenta
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
