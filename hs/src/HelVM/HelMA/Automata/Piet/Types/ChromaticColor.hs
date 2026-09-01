module HelVM.HelMA.Automata.Piet.Types.ChromaticColor
  ( ChromaticColor (..)
  , chromaticDiff
  , diffColor
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

chromaticDiff ∷ (Hue, Lightness) → (Hue, Lightness) → Int
chromaticDiff (h1, l1) (h2, l2) = hueDiff h2 h1 * 3 + lightnessDiff l2 l1

diffColor ∷ ChromaticColor → ChromaticColor → ChromaticColor
diffColor (ChromaticColor fromLight fromColor) (ChromaticColor toLight toColor) = ChromaticColor (brightnessChange fromLight toLight) (hueChange fromColor toColor)

data ChromaticColor
  = ChromaticColor !Lightness !Hue --FIXME revert parameters
  deriving stock (Eq, Ord, Read, Show)
