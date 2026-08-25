module HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Position
  ( move
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax

move ∷ Enum a ⇒ DirectionPointer → (a, a) → (a, a)
move DPRight = first  succ
move DPDown  = second succ
move DPLeft  = first  pred
move DPUp    = second pred
