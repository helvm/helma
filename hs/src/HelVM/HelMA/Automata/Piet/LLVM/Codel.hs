module HelVM.HelMA.Automata.Piet.LLVM.Codel
  ( Codel (..)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor

data Codel
  = AchromaticCodel ChromaticColor
  | WhiteCodel
  | BlackCodel
  deriving stock (Eq, Ord, Show)
