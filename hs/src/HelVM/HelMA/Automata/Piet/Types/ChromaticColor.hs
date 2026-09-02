module HelVM.HelMA.Automata.Piet.Types.ChromaticColor
  ( ChromaticColor (..)
  , chromaticChange
  , chromaticDiffToIndex
  , hueL
  , lightnessL
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Relude.Extra

-- TYPES

data ChromaticColor
  = ChromaticColor
      { hue       :: !Hue
      , lightness :: !Lightness
      }
  deriving stock (Eq, Ord, Read, Show)

-- LENSES

hueL ∷ Lens' ChromaticColor Hue
hueL = lens hue $ \s x → s { hue = x }

lightnessL ∷ Lens' ChromaticColor Lightness
lightnessL = lens lightness $ \s x → s { lightness = x }

-- FUNCTIONS

chromaticDiffToIndex ∷ (Hue, Lightness) → (Hue, Lightness) → Int
chromaticDiffToIndex (h1, l1) (h2, l2) = hueDiff h1 h2 * 3 + lightnessDiff l1 l2

chromaticChange ∷ ChromaticColor → ChromaticColor → ChromaticColor
chromaticChange (ChromaticColor h1 l1) (ChromaticColor h2 l2) =
  ChromaticColor (hueChange h1 h2) (lightnessChange l1 l2)
