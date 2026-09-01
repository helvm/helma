module HelVM.HelMA.Automata.Piet.LLVM.Internal.Position
  ( move
  ) where

import qualified HelVM.HelMA.Automata.Piet.Types.DirectionPointer as DP

import           Relude.Extra                                     ( next, prev )

move ∷ Eq a ⇒ Bounded a ⇒ Enum a ⇒ DP.DirectionPointer → (a, a) → (a, a)
move DP.DPRight = first  next
move DP.DPDown  = second next
move DP.DPLeft  = first  prev
move DP.DPUp    = second prev
