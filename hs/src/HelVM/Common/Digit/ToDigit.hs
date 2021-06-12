module HelVM.Common.Digit.ToDigit where

import HelVM.Common.Digit.Digits
import HelVM.Common.Safe

import Data.Char hiding (chr)

import qualified Data.List.Split as List

makeDigitString :: ToDigit a => [a] -> Safe String
makeDigitString xs = sequenceA $ toDigitChar <$> xs

makeAsciiString :: ToDigit a => Int -> Int -> [a] -> Safe String
makeAsciiString base n xs = sequenceA $ makeChar base <$> List.chunksOf n xs

makeChar :: ToDigit a => Int -> [a] -> Safe Char
makeChar base xs = chr <$> makeIntegral base (reverse xs)

toDigitChar :: ToDigit a => a -> Safe Char
toDigitChar a = integerToDigit <$> toDigit a

integerToDigit :: Integer -> Char
integerToDigit = intToDigit . fromInteger

makeIntegral :: (ToDigit a , Integral b) => b -> [a] -> Safe b
makeIntegral base digits = digitsToIntegral base (toDigit <$> digits)

class ToDigit t where
  toDigit :: Integral a => t -> Safe a
