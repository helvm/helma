module HelVM.Common.Digit.Digitable where

import           HelVM.Common.Digit.Digits
import           HelVM.Common.Digit.ToDigit

import           HelVM.Common.Safe

-- | Public functions

naturalToDL :: (MonadSafeError m , Digitable a) => Natural -> m [a]
naturalToDL n = liftSafe $ sequenceA $ fromDigit <$> naturalToDigits2 n

stringToDL :: (MonadSafeError m , Digitable a) => String -> m [a]
stringToDL s = join <$> sequenceA (charToDL <$> s)

charToDL :: (MonadSafeError m, Digitable a) => Char -> m [a]
charToDL c = sequenceA $ fromDigit <$> toBits8 (ord c `mod` 256)

-- | Type Classes

class ToDigit t => Digitable t where
  fromDigit :: (MonadSafeError m , Show a , Integral a) => a -> m t

-- | Internal functions

toBits8 :: Int -> [Natural]
toBits8 = toBitsBySize 8

toBitsBySize :: Int -> Int -> [Natural]
toBitsBySize 0    _ = []
toBitsBySize size 0 = [0 | _ <- [1..size]]
toBitsBySize size x
 | k == 0    = 0 : toBitsBySize maxDigit x
 | otherwise = 1 : toBitsBySize maxDigit (x - k*m)
    where
      maxDigit = size - 1
      m = 2 ^ maxDigit
      k = x `div` m