module HelVM.HelMA.Automata.Piet.LLVM.Internal.Position
  ( move
  ) where

-- import           Control.Arrow
import           HelVM.HelMA.Automata.Piet.LLVM.Syntax

move ∷ Enum a ⇒ DirectionPointer → (a, a) → (a, a)
move DPRight = first  succ
move DPDown  = second succ
move DPLeft  = first  pred
move DPUp    = second pred
