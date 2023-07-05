module HelVM.HelMA.Automata.Piet.DirectionPointer where

rotate :: (Bounded a , Enum a) => Int -> a -> a
rotate n x = toEnum $ (fromEnum x + n) `mod` (fromEnum maxBound_ - fromEnum minBound_ + 1)
  where
    maxBound_ = asTypeOf maxBound x
    minBound_ = asTypeOf minBound x

data DirectionPointer = DPRight | DPDown | DPLeft  | DPUp
  deriving stock (Bounded , Show, Read, Eq, Ord, Enum)
