module HelVM.Common.Digit.Digits (
  digitsToIntegral,
  digitsToIntegral',
  naturalToDigits7,
  naturalToDigits2,
) where

import HelVM.Common.Safe

digitsToIntegral :: Integral a => a -> [Safe a] -> Safe a --FIXME
digitsToIntegral base = foldr (liftedMulAndAdd base) (safe 0)

digitsToIntegral' :: Integral a => a -> [a] -> a
digitsToIntegral' base = foldr (mulAndAdd base) 0

liftedMulAndAdd :: (Integral a) => a -> Safe a -> Safe a -> Safe a
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

divModMaybe :: Natural -> Natural -> Maybe (Natural, Natural)
divModMaybe _    0     = Nothing
divModMaybe base value = Just (value `divMod` base)

unfoldl :: (a -> Maybe (a,b)) -> a -> [b]
unfoldl lambda value = check $ lambda value where
  check  Nothing     = []
  check (Just (a , b)) = unfoldl lambda a <> [b]
