module HelVM.HelMA.Automata.Piet.Types.Cyclic
  ( cyclicChange
  , cyclicDiff
  , cyclicMove
  , cyclicSucc
  ) where

import           HelVM.HelIO.SwitchEnum

-- FUNCTIONS

cyclicSucc ∷ (Bounded a, Enum a) ⇒ a → a
cyclicSucc = cyclicMove 1

cyclicMove ∷ (Bounded e, Enum e) ⇒ Int → e → e
cyclicMove n e = unsafeEnum $ (fromEnum e + n) `mod` cardinalityOf e

cyclicChange ∷ (Bounded e, Enum e) ⇒ e → e → e
cyclicChange e1 e2 = unsafeEnum $ cyclicDiff e1 e2

cyclicDiff ∷ (Bounded e, Enum e) ⇒ e → e → Int
cyclicDiff e1 e2 = (fromEnum e2 - fromEnum e1) `mod` cardinalityOf e1

cardinalityOf ∷ (Bounded a, Enum a) ⇒ a → Int
cardinalityOf x = fromEnum (maxBound `asTypeOf` x) - fromEnum (minBound `asTypeOf` x) + 1
