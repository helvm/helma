module HelVM.HelMA.Automata.Piet.LLVM.Codel
  ( Codel (..)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

data Codel
  = AchromaticCodel Hue Lightness
  | WhiteCodel
  | BlackCodel
  deriving stock (Eq, Ord, Show)
