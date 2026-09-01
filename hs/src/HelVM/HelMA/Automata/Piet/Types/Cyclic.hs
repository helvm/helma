module HelVM.HelMA.Automata.Piet.Types.Cyclic
  ( cyclicChange
  , cyclicDiff
  , cyclicMove
  , cyclicSucc
  ) where

import           HelVM.HelIO.SwitchEnum

-- FUNCTIONS

cyclicSucc ∷ (Bounded a, Enum a) ⇒ a → a
cyclicSucc a = cyclicMove (fromEnum (maxBound `asTypeOf` a) + 1) 1 a

cyclicMove ∷ (Bounded e, Enum e) ⇒ Int → Int → e → e
cyclicMove i n e = unsafeEnum $ (fromEnum e + n) `mod` i

cyclicChange ∷ (Bounded e, Enum e) ⇒ Int → e → e → e
cyclicChange i e1 e2 = unsafeEnum $ cyclicDiff i e1 e2

cyclicDiff ∷ Enum e ⇒ Int → e → e → Int
cyclicDiff i e1 e2 = (fromEnum e2 - fromEnum e1) `mod` i
