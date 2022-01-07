module HelVM.HelMA.Automaton.Units.RAM (
  genericLoad,
  load,
  storeChar,
  genericStore,
  store,
  fromList,
  RAM,
) where

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Containers.LLInsertDef

--import           HelVM.HelMA.Automaton.BIntegral
----import           HelVM.HelMA.Automaton.Dynamic.DynamicIntegral
--
--import           HelVM.HelIO.Containers.LLIndexSafe
--import           HelVM.HelIO.Containers.LLInsertDef
--import           HelVM.HelIO.Control.Safe
--import           HelVM.HelIO.ReadText

import           Data.Default
import           Prelude                            hiding (divMod, drop, splitAt, swap)

genericLoad :: (Integral i , RAM ll element) => ll -> i -> element
genericLoad l = load l . fromIntegral

load :: (RAM ll element) => ll -> Address -> element
load l i = indexMaybe l i ?: def

storeChar :: (Num element , Integral address , RAM ll element) => address -> Char -> ll -> ll
storeChar a char = genericStore a $ ord char

--storeNum :: (BIntegral element , RAM ll element) => element -> Text -> ll -> Safe ll
--storeNum a line l = store' <$> readTextSafe line where store' e = store a e l

genericStore :: (Integral value , Num element , Integral address , RAM ll element) => address -> value -> ll -> ll
genericStore a v = store a $ fromIntegral v

store :: (Integral a , RAM ll element) => a -> element -> ll -> ll
store = insertDef . fromIntegral

-- | Types
type RAM ll element = (Show ll , Default element , II ll element)

type II ll element = (InsertDef ll element , IndexSafe ll element)

type Address = Int
