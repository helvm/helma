module HelVM.Common.Digit.ToDigit (
  makeDigitStringFromList,
  makeDigitString,
  makeAsciiText28FromList,
  makeAsciiText28,
  makeAsciiString28FromList,
  makeAsciiString28,
  makeAsciiString,
  makeIntegral7FromList,
  makeIntegral7,
  makeIntegral2FromList,
  makeIntegral2,
  makeIntegral,
  wrongToken,
  ToDigit,
  toDigit,
) where

import           HelVM.Common.Digit.Digits

import           HelVM.Common.Control.Safe

import           Data.Char                      hiding (chr)

import qualified HelVM.Common.Collections.SList as S

import qualified Data.ListLike                  as LL

makeDigitStringFromList :: (MonadSafe m , ToDigit a) => [a] -> m S.SString
makeDigitStringFromList = makeDigitString . fromList

makeDigitString :: (MonadSafe m , ToDigit a) => S.SList a -> m S.SString
makeDigitString = traverse toDigitChar

makeAsciiText28FromList :: (MonadSafe m , ToDigit a) => [a] -> m Text
makeAsciiText28FromList = makeAsciiText28 . fromList

makeAsciiText28 :: (MonadSafe m , ToDigit a) => S.SList a -> m Text
makeAsciiText28 l = toText . S.sListToList <$> makeAsciiString28 l

makeAsciiString28FromList :: (MonadSafe m , ToDigit a) => [a] -> m S.SString
makeAsciiString28FromList = makeAsciiString28 . fromList

makeAsciiString28 :: (MonadSafe m , ToDigit a) => S.SList a -> m S.SString
makeAsciiString28 = makeAsciiString 2 8

makeAsciiString :: (MonadSafe m , ToDigit a) => Int -> Int -> S.SList a -> m S.SString
makeAsciiString base n xs = traverse (makeChar base) $ S.chunksOf n xs

makeChar :: (MonadSafe m , ToDigit a) => Int -> S.SList a -> m Char
makeChar base xs = chr <$> makeIntegral base (LL.reverse xs)

toDigitChar :: MonadSafe m => ToDigit a => a -> m Char
toDigitChar a = integerToDigit <$> toDigit a

integerToDigit :: Integer -> Char
integerToDigit = intToDigit . fromInteger

makeIntegral7FromList :: (MonadSafe m , ToDigit a, Integral b) => [a] -> m b
makeIntegral7FromList = makeIntegral7 . fromList

makeIntegral7 :: (MonadSafe m , ToDigit a , Integral b) => S.SList a -> m b
makeIntegral7 = makeIntegral 7

makeIntegral2FromList :: (MonadSafe m , ToDigit a, Integral b) => [a] -> m b
makeIntegral2FromList = makeIntegral2 . fromList

makeIntegral2 :: (MonadSafe m , ToDigit a , Integral b) => S.SList a -> m b
makeIntegral2 = makeIntegral 2

makeIntegral :: (MonadSafe m , ToDigit a , Integral b) => b -> S.SList a -> m b
makeIntegral base digits = digitsToIntegral base (toDigit <$> digits)

wrongToken :: (MonadSafe m , Show t) => t -> m a
wrongToken = liftErrorWithPrefix "Wrong token" . show

class ToDigit t where
  toDigit :: (MonadSafe m, Integral a) => t -> m a
