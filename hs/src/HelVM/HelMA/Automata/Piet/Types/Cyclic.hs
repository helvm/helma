module HelVM.HelMA.Automata.Piet.Types.Cyclic
  ( change
  , changeDiff
  , cyclicSucc
  , changeDiffAsInt
  ) where

import           HelVM.HelIO.SwitchEnum

-- FUNCTIONS

cyclicSucc ∷ (Bounded a, Enum a) ⇒ a → a
cyclicSucc a = change (fromEnum (maxBound `asTypeOf` a) + 1) 1 a

change ∷ (Bounded e, Enum e, Enum a) ⇒ Int → Int → a → e
change i n e = unsafeEnum $ (fromEnum e + n) `mod` i

changeDiff ∷ (Bounded e, Enum e) ⇒ Int → e → e → e
changeDiff i e1 e2 = unsafeEnum $ changeDiffAsInt i e1 e2

changeDiffAsInt ∷ Enum e ⇒ Int → e → e → Int
changeDiffAsInt i e1 e2 = (fromEnum e2 - fromEnum e1) `mod` i
