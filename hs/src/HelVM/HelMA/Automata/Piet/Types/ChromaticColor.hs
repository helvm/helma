module HelVM.HelMA.Automata.Piet.Types.ChromaticColor
  ( ChromaticColor (..)
  , diffColor
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Brightness
import           HelVM.HelMA.Automata.Piet.Types.Hue

diffColor ∷ ChromaticColor → ChromaticColor → ChromaticColor
diffColor (ChromaticColor fromLight fromColor) (ChromaticColor toLight toColor) = ChromaticColor (brightnessChange fromLight toLight) (hueChange fromColor toColor)

data ChromaticColor = ChromaticColor !Brightness !Hue
  deriving stock (Show, Read, Eq, Ord)
