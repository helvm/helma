module HelVM.Common.Digit.Digits (
  digitsToIntegral,
  naturalToDigits7,
  naturalToDigits2,
) where

import           HelVM.Common.Collections.SList
import           HelVM.Common.Control.Safe

digitsToIntegral :: (MonadSafe m , Integral a) => a -> SList (m a) -> m a
digitsToIntegral base = foldr (liftedMulAndAdd base) (pure 0)

liftedMulAndAdd :: (MonadSafe m , Integral a)  => a -> m a -> m a -> m a
liftedMulAndAdd base = liftA2 (mulAndAdd base)

mulAndAdd :: Integral a => a -> a -> a -> a
mulAndAdd base digit acc = acc * base + digit

----

naturalToDigits7 :: Natural -> [Natural]
naturalToDigits7 = naturalToDigits 7

naturalToDigits2 :: Natural -> [Natural]
naturalToDigits2 = naturalToDigits 2

naturalToDigits :: Natural -> Natural -> [Natural]
naturalToDigits base = unfoldl (divModMaybe base)

divModMaybe :: Natural -> Natural -> Maybe (Natural , Natural)
divModMaybe _    0     = Nothing
divModMaybe base value = Just (value `divMod` base)

unfoldl :: (a -> Maybe (a , b)) -> a -> [b]
unfoldl lambda = check . lambda where
  check  Nothing       = []
  check (Just (a , b)) = unfoldl lambda a <> [b]
