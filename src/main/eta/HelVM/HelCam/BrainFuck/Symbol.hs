module HelVM.HelCam.BrainFuck.Symbol where

import Data.Char
import Data.Int
import Data.Word

class (Num a, Eq a) => Symbol a where
  blank      :: a
  fromChar   :: Char -> a
  toChar     :: a -> Char
  succSymbol :: a -> a
  predSymbol :: a -> a

--

instance Symbol Int where
  blank      = 0
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral
  succSymbol = succMod
  predSymbol = predMod

instance Symbol Word where
  blank      = 0
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral
  succSymbol = succMod
  predSymbol = predMod

instance Symbol Int8 where
  blank      = 0
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral
  succSymbol = (+1)
  predSymbol = subtract 1

instance Symbol Word8 where
  blank      = 0
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral
  succSymbol = (+1)
  predSymbol = subtract 1

--

countSymbols :: (Integral a) => a
countSymbols = 256

modifyMod :: (Integral a) => (a -> a) -> a -> a
modifyMod f i = f (i + countSymbols) `mod` countSymbols

normalizeMod :: (Integral a) => a -> a
normalizeMod = modifyMod id

succMod :: (Integral a) => a -> a
succMod = modifyMod succ

predMod :: (Integral a) => a -> a
predMod = modifyMod pred

modifyChar :: (Int -> Int) -> Char -> Char
modifyChar modify = chr . modify . ord
