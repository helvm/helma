module HelVM.HelMA.Automata.Piet.Types.ChromaticColor
  ( ChromaticColor (..)
  , chromaticDiff
  , chromaticDiffToIndex
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

chromaticDiffToIndex ∷ (Hue, Lightness) → (Hue, Lightness) → Int
chromaticDiffToIndex (h1, l1) (h2, l2) = hueDiff h1 h2 * 3 + lightnessDiff l1 l2

chromaticDiff ∷ ChromaticColor → ChromaticColor → ChromaticColor
chromaticDiff (ChromaticColor h1 l1) (ChromaticColor h2 l2) = ChromaticColor (hueChange h1 h2) (lightnessChange l1 l2)

data ChromaticColor
  = ChromaticColor !Hue !Lightness
  deriving stock (Eq, Ord, Read, Show)
