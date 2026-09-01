module HelVM.HelMA.Automata.Piet.Types.Hue
  ( Hue (..)
  , calculate
  , diffHue
  , hueChange
  , hueDiff
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Cyclic

hueDiff ∷ Hue → Hue → Int
hueDiff h1 h2 = (fromEnum h1 - fromEnum h2) `mod` 6

diffHue ∷ Hue → Hue → Int
diffHue h1 h2 = (fromEnum h2 - fromEnum h1) `mod` 6

calculate ∷ Maybe Hue → Maybe Hue → Maybe Int
calculate (Just h1) (Just h2) = Just $ (fromEnum h2 - fromEnum h1 + 6) `mod` 6
calculate _        _          = Nothing

hueChange ∷ Hue → Hue → Hue
hueChange = changeDiff 6

data Hue
  = Red
  | Yellow
  | Green
  | Cyan
  | Blue
  | Magenta
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
