module HelVM.Common.Digit.ToDigit where

import HelVM.Common.Digit.Digits
import HelVM.Common.Safe

import Data.Char hiding (chr)

import qualified Data.List.Split as List

makeDigitString :: (MonadSafeError m , ToDigit a) => [a] -> m String
makeDigitString xs = sequenceA $ toDigitChar <$> xs

makeAsciiString :: (MonadSafeError m , ToDigit a) => Int -> Int -> [a] -> m String
makeAsciiString base n xs = sequenceA $ makeChar base <$> List.chunksOf n xs

makeChar :: (MonadSafeError m , ToDigit a) => Int -> [a] -> m Char
makeChar base xs = chr <$> makeIntegral base (reverse xs)

toDigitChar :: MonadSafeError m => ToDigit a => a -> m Char
toDigitChar a = integerToDigit <$> toDigit a

integerToDigit :: Integer -> Char
integerToDigit = intToDigit . fromInteger

makeIntegral :: (MonadSafeError m , ToDigit a , Integral b) => b -> [a] -> m b
makeIntegral base digits = digitsToIntegral base (toDigit <$> digits)

class ToDigit t where
  toDigit :: (MonadSafeError m, Integral a) => t -> m a
