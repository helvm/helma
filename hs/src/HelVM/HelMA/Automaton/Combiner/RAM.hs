module HelVM.HelMA.Automaton.Combiner.RAM (
  genericLoad,
  load,
  flippedStoreChar,
  storeChar,
  genericStore,
  store,
  fromList,
  RAM,
) where

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Containers.LLInsertDef

import           Data.Default
import           Prelude                            hiding (divMod, drop, splitAt, swap)

genericLoad :: (Integral i , RAM ll element) => ll -> i -> element
genericLoad l = load l . fromIntegral

load :: (RAM ll element) => ll -> Address -> element
load l i = indexMaybe l i ?: def

flippedStoreChar :: (Num element , Integral address , RAM ll element) => address -> ll -> Char -> ll
flippedStoreChar a = flip (storeChar a)

storeChar :: (Num element , Integral address , RAM ll element) => address -> Char -> ll -> ll
storeChar a char = genericStore a $ ord char

genericStore :: (Integral value , Num element , Integral address , RAM ll element) => address -> value -> ll -> ll
genericStore a v = store a $ fromIntegral v

store :: (Integral a , RAM ll element) => a -> element -> ll -> ll
store = insertDef . fromIntegral

-- | Types
type RAM ll element = (Show ll , Default element , II ll element)

type II ll element = (InsertDef ll element , IndexSafe ll element)

type Address = Int
