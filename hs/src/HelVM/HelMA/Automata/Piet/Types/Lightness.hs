module HelVM.HelMA.Automata.Piet.Types.Lightness
  ( Lightness (..)
  , lightnessChange
  , lightnessDiff
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Cyclic

lightnessDiff ∷ Lightness → Lightness → Int
lightnessDiff = cyclicDiff 3

lightnessChange ∷ Lightness → Lightness → Lightness
lightnessChange = cyclicChange 3

data Lightness
  = Light
  | Normal
  | Dark
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
