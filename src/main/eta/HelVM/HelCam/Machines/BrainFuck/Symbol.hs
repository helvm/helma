module HelVM.HelCam.Machines.BrainFuck.Symbol where

import Relude.Extra

import Data.Default

class (Bounded a, Default a, Enum a, Eq a, Num a) => Symbol a where
  fromChar   :: Char -> a
  toChar     :: a -> Char

  blank      :: a
  blank      = def
  succSymbol :: a -> a
  succSymbol = next
  predSymbol :: a -> a
  predSymbol = prev

--

instance Symbol Int where
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Word where
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int8 where
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word8 where
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

--

countSymbols :: (Integral a) => a
countSymbols = 256

modifyMod :: (Integral a) => (a -> a) -> a -> a
modifyMod f i = f (i + countSymbols) `mod` countSymbols

normalizeMod :: (Integral a) => a -> a
normalizeMod = modifyMod id

modifyChar :: (Int -> Int) -> Char -> Char
modifyChar f = chr . f . ord
