module HelVM.HelMA.Automaton.Operator.Util where

integralToBool :: Integral a => a -> Bool
integralToBool a = a /= 0

boolToIntegral :: Integral a => Bool -> a
boolToIntegral = fromIntegral . fromEnum

boolToBits :: Integral a => Bool -> a
boolToBits False =  0
boolToBits True  = -1
