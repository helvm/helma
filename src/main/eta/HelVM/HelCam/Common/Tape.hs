module HelVM.HelCam.Common.Tape where

import HelVM.HelCam.Common.Util

type FullTape value = (HalfTape value, HalfTape value)
type FullTapeD value = D (FullTape value)

loadFromFullTape :: (Integral address, Num value) => FullTape value -> address -> value
loadFromFullTape (left,right) a
  | 0 <= a    = loadFromHalfTape' right  a
  | otherwise = loadFromHalfTape' left (-a)

storeToFullTape :: (Integral address, Num value) => address -> value -> FullTapeD value
storeToFullTape a v (left, right)
  | 0 <= a    = (left, right')
  | otherwise = (left', right)
  where
    right' = storeToHalfTape'   a  v right
    left'  = storeToHalfTape' (-a) v left

----

type HalfTape value = [value]
type HalfTapeD value = D (HalfTape value)

loadFromHalfTape :: (Show address, Integral address, Num value) => HalfTape value -> address -> value
loadFromHalfTape tape a
  | 0 <= a    = loadFromHalfTape' tape a
  | otherwise = errorWithoutStackTrace $ "loadFromHalfTape: negative argument " ++ show a

storeToHalfTape :: (Show address, Integral address, Num value) => address -> value -> HalfTapeD value
storeToHalfTape a
  | 0 <= a    = storeToHalfTape' a
  | otherwise = errorWithoutStackTrace $ "storeToHalfTape: negative argument " ++ show a

----

loadFromHalfTape' :: (Integral address, Num value) => HalfTape value -> address -> value
loadFromHalfTape'    []  _ = 0
loadFromHalfTape' (x:_)  0 = x
loadFromHalfTape' (_:xs) a = loadFromHalfTape' xs (a-1)

storeToHalfTape' :: (Integral address, Num value) => address -> value -> HalfTapeD value
storeToHalfTape' 0 v []     = [v]
storeToHalfTape' 0 v (_:xs) = v : xs
storeToHalfTape' a v []     = 0 : storeToHalfTape' (a-1) v []
storeToHalfTape' a v (x:xs) = x : storeToHalfTape' (a-1) v xs
