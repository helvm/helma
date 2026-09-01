module HelVM.HelMA.Automata.Piet.LLVM.Codel
  ( Codel (..)
  ) where

import HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

data Codel
  = AchromaticCodel ChromaticColor
  | WhiteCodel
  | BlackCodel
  deriving stock (Eq, Ord, Show)
