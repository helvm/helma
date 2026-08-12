module HelVM.HelMA.Automata.Piet.Types.ChromaticColor
  ( ChromaticColor (..)
  , diffColor
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

diffColor ∷ ChromaticColor → ChromaticColor → ChromaticColor
diffColor (ChromaticColor fromLight fromColor) (ChromaticColor toLight toColor) = ChromaticColor (brightnessChange fromLight toLight) (hueChange fromColor toColor)

data ChromaticColor
  = ChromaticColor !Lightness !Hue
  deriving stock (Eq, Ord, Read, Show)
