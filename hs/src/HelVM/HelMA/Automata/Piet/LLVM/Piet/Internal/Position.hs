module HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Position
  ( move
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax

import           Relude.Extra                               ( next, prev )

move ∷ Eq a ⇒ Bounded a ⇒ Enum a ⇒ DirectionPointer → (a, a) → (a, a)
move DPRight = first  next
move DPDown  = second next
move DPLeft  = first  prev
move DPUp    = second prev
