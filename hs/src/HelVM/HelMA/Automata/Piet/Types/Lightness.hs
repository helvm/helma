module HelVM.HelMA.Automata.Piet.Types.Lightness
  ( Lightness (..)
  , brightnessChange
  , diffLightness
  , lightnessDiff
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Cyclic

lightnessDiff ∷ Lightness → Lightness → Int
lightnessDiff l1 l2 = (fromEnum l2 - fromEnum l1) `mod` 3

diffLightness ∷ Lightness → Lightness → Int
diffLightness l1 l2 = (fromEnum l2 - fromEnum l1) `mod` 3

brightnessChange ∷ Lightness → Lightness → Lightness
brightnessChange = changeDiff 3

data Lightness
  = Light
  | Normal
  | Dark
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
