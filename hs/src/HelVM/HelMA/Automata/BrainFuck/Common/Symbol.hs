module HelVM.HelMA.Automata.BrainFuck.Common.Symbol (
  inc,
  compare0,
  def,
  next,
  prev,
  toInteger,
  fromChar,
  toChar,
  Symbol,
) where

import           Data.Default (Default)

import qualified Data.Default as Default
import qualified Relude.Extra as Extra

inc :: Symbol e => e -> e -> e
inc = flip (+)

compare0 :: Integer -> Ordering
compare0 = compare 0

--

def :: Symbol e => e
def = Default.def

next :: Symbol e => e -> e
next = Extra.next

prev :: Symbol e => e -> e
prev = Extra.prev

class (Bounded e , Default e , Enum e , Eq e , Integral e , Show e) => Symbol e where
--  toInteger  :: e -> Integer
  fromChar   :: Char -> e
  toChar     :: e -> Char

--

instance Symbol Int where
--  toInteger  = fromIntegral
  fromChar   = ord
  toChar     = chr

instance Symbol Word where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int8 where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word8 where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int16 where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word16 where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int32 where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word32 where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral

instance Symbol Int64 where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . normalizeMod . fromIntegral

instance Symbol Word64 where
--  toInteger  = fromIntegral
  fromChar   = fromIntegral . ord
  toChar     = chr . fromIntegral
--

countSymbols :: (Integral e) => e
countSymbols = 256

modifyMod :: (Integral e) => (e -> e) -> e -> e
modifyMod f i = f (i + countSymbols) `mod` countSymbols

normalizeMod :: (Integral e) => e -> e
normalizeMod = modifyMod id
