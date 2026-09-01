module HelVM.HelMA.Automata.Piet.Types.Hue
  ( Hue (..)
  , hueChange
  , hueDiff
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Cyclic

hueDiff ∷ Hue → Hue → Int
hueDiff = cyclicDiff 6

hueChange ∷ Hue → Hue → Hue
hueChange = cyclicChange 6

data Hue
  = Red
  | Yellow
  | Green
  | Cyan
  | Blue
  | Magenta
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
