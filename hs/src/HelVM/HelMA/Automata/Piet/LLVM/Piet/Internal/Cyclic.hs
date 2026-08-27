{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use next" #-}

module HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.Cyclic
  ( cyclicSucc
  ) where

cyclicSucc ∷ (Eq a, Enum a, Bounded a) ⇒ a → a
cyclicSucc x | x == maxBound = minBound
             | otherwise     = succ x
