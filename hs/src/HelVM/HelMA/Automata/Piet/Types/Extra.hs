module HelVM.HelMA.Automata.Piet.Types.Extra where

import           HelVM.HelIO.SwitchEnum

change :: (Bounded e, Enum e, Enum a) => Int -> Int -> a -> e
change i n e = unsafeEnum $ (fromEnum e + n) `mod` i

changeDiff :: (Bounded e, Enum e) => Int -> e -> e -> e
changeDiff i e1 e2 = unsafeEnum $ (fromEnum e2 - fromEnum e1) `mod` i
