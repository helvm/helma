module HelVM.HelMA.Automata.Piet.Common.EnumExtra where

import           HelVM.HelIO.SwitchEnum

sumEnums :: (Bounded a , Enum a) => a -> a -> a
sumEnums e = rotateEnum (fromEnum e)

rotateEnum :: (Bounded a , Enum a) => Int -> a -> a
rotateEnum n x = unsafeEnum $ (fromEnum x + n) `mod` (fromEnum maxBound_ - fromEnum minBound_ + 1)
  where
    maxBound_ = asTypeOf maxBound x
    minBound_ = asTypeOf minBound x
