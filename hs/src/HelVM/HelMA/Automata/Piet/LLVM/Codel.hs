module HelVM.HelMA.Automata.Piet.LLVM.Codel
  ( Codel (..)
  ) where

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor

data Codel
  = Chromatic ChromaticColor
  | White
  | Black
  deriving stock (Eq, Ord, Show)
