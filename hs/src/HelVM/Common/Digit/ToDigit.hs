module HelVM.Common.Digit.ToDigit (
  makeDigitStringFromList,
  makeDigitString,
  makeAsciiString28FromList,
  makeAsciiString28,
  makeAsciiString,
  makeIntegral7FromList,
  makeIntegral7,
  makeIntegral2FromList,
  makeIntegral2,
  makeIntegral,
  ToDigit,
  toDigit,
) where

import           HelVM.Common.Digit.Digits

import           HelVM.Common.Collections.SList
import           HelVM.Common.Safe

import           Data.Char                      hiding (chr)

import qualified HelVM.Common.Collections.SList as SList

import qualified Data.ListLike                  as LL

makeDigitStringFromList :: (MonadSafeError m , ToDigit a) => [a] -> m SString
makeDigitStringFromList = makeDigitString . fromList

makeDigitString :: (MonadSafeError m , ToDigit a) => SList a -> m SString
makeDigitString = traverse toDigitChar

makeAsciiString28FromList :: (MonadSafeError m , ToDigit a) => [a] -> m SString
makeAsciiString28FromList = makeAsciiString28 . fromList

makeAsciiString28 :: (MonadSafeError m , ToDigit a) => SList a -> m SString
makeAsciiString28 = makeAsciiString 2 8

makeAsciiString :: (MonadSafeError m , ToDigit a) => Int -> Int -> SList a -> m SString
makeAsciiString base n xs = traverse (makeChar base) $ SList.chunksOf n xs

makeChar :: (MonadSafeError m , ToDigit a) => Int -> SList a -> m Char
makeChar base xs = chr <$> makeIntegral base (LL.reverse xs)

toDigitChar :: MonadSafeError m => ToDigit a => a -> m Char
toDigitChar a = integerToDigit <$> toDigit a

integerToDigit :: Integer -> Char
integerToDigit = intToDigit . fromInteger

makeIntegral7FromList :: (MonadSafeError m , ToDigit a, Integral b) => [a] -> m b
makeIntegral7FromList = makeIntegral7 . fromList

makeIntegral7 :: (MonadSafeError m , ToDigit a , Integral b) => SList a -> m b
makeIntegral7 = makeIntegral 7

makeIntegral2FromList :: (MonadSafeError m , ToDigit a, Integral b) => [a] -> m b
makeIntegral2FromList = makeIntegral2 . fromList

makeIntegral2 :: (MonadSafeError m , ToDigit a , Integral b) => SList a -> m b
makeIntegral2 = makeIntegral 2

makeIntegral :: (MonadSafeError m , ToDigit a , Integral b) => b -> SList a -> m b
makeIntegral base digits = digitsToIntegral base (toDigit <$> digits)

class ToDigit t where
  toDigit :: (MonadSafeError m, Integral a) => t -> m a
