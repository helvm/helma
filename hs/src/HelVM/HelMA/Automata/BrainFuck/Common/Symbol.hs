module HelVM.HelMA.Automata.BrainFuck.Common.Symbol (
  inc,
  compare0,
  def,
  next,
  prev,
  fromChar,
  toChar,
  Symbol,
) where

import           Control.Monad.Extra

import           Data.Default        (Default)

import qualified Data.Default        as Default
import qualified Relude.Extra        as Extra

inc :: Symbol e => Int -> e -> e
inc i e = loop atc (i , e) where
  atc (i' , e') = (check . compare0) i' where
    check LT = Left (i' - 1 , next e')
    check GT = Left (i' + 1 , prev e')
    check EQ = Right e'

compare0 :: Int -> Ordering
compare0 = compare 0

--

def :: Symbol e => e
def = Default.def

next :: Symbol e => e -> e
next = Extra.next

prev :: Symbol e => e -> e
prev = Extra.prev

class (Bounded e , Default e , Enum e , Eq e , Num e , Show e) => Symbol e where
  fromChar   :: Char -> e
  toChar     :: e -> Char

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

countSymbols :: (Integral e) => e
countSymbols = 256

modifyMod :: (Integral e) => (e -> e) -> e -> e
modifyMod f i = f (i + countSymbols) `mod` countSymbols

normalizeMod :: (Integral e) => e -> e
normalizeMod = modifyMod id
