module HelVM.HelMA.Automata.Piet.Types.Brightness (
  brightnessChange,
  Brightness(..),
) where

import           HelVM.HelMA.Automata.Piet.Types.Extra

brightnessChange ∷ Brightness → Brightness → Brightness
brightnessChange = changeDiff 3

data Brightness = Light | Normal | Dark
  deriving stock (Bounded , Show , Read, Eq , Ord , Enum)
