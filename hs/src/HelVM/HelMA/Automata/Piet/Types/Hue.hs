module HelVM.HelMA.Automata.Piet.Types.Hue (
  hueChange,
  Hue(..),
) where

import           HelVM.HelMA.Automata.Piet.Types.Extra

hueChange ∷ Hue → Hue → Hue
hueChange = changeDiff 6

data Hue = Red | Yellow | Green | Cyan | Blue | Magenta
  deriving stock (Bounded , Show , Read, Eq , Ord , Enum)
