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
chromaticDiff (ChromaticColor l1 h1) (ChromaticColor l2 h2) = ChromaticColor (lightnessChange l1 l2) (hueChange h1 h2)

data ChromaticColor
  = ChromaticColor !Lightness !Hue --FIXME revert parameters
  deriving stock (Eq, Ord, Read, Show)
