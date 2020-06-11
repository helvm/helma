module HelVM.HelMA.Automata.BrainFuck.Symbol (
  Symbol,
  HelVM.HelMA.Automata.BrainFuck.Symbol.def,
  HelVM.HelMA.Automata.BrainFuck.Symbol.next,
  HelVM.HelMA.Automata.BrainFuck.Symbol.prev,
  fromChar,
  toChar
) where

import Relude.Extra as Extra

import Data.Default as Default

def :: Symbol s => s
def = Default.def

next :: Symbol s => s -> s
next = Extra.next

prev :: Symbol s => s -> s
prev = Extra.prev

class (Bounded a, Default a, Enum a, Eq a, Num a, Show a) => Symbol a where
  fromChar   :: Char -> a
  toChar     :: a -> Char

--

instance Symbol Int where
  fromChar   = ord
  toChar     = chr

instance Symbol Word where
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int8 where
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word8 where
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int16 where
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word16 where
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int32 where
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word32 where
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int64 where
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word64 where
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral
--

countSymbols :: (Integral a) => a
countSymbols = 256

modifyMod :: (Integral a) => (a -> a) -> a -> a
modifyMod f i = f (i + countSymbols) `mod` countSymbols

normalizeMod :: (Integral a) => a -> a
normalizeMod = modifyMod id
