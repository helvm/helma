module HelVM.HelMA.Automata.Piet.Types.Hue
  ( Hue (..)
  , calculate
  , hueChange
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Cyclic

calculate ∷ Maybe Hue → Maybe Hue → Maybe Int
calculate (Just x) (Just y) = Just $ (fromEnum y - fromEnum x + 6) `mod` 6
calculate _        _        = Nothing

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
