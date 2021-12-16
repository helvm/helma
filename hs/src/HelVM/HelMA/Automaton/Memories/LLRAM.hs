module HelVM.HelMA.Automaton.Memories.LLRAM (
  genericLoad,
  load,
  storeNum,
  storeChar,
  genericStore,
  store,
  fromList,
  RAM,
) where

import           HelVM.Common.Containers.LLIndexSafe
import           HelVM.Common.Containers.LLInsertDef
import           HelVM.Common.Control.Safe
import           HelVM.Common.ReadText

import           Data.Default
import           Prelude                             hiding (divMod, drop, splitAt, swap)

genericLoad :: (Integral i , RAM ll element) => ll -> i -> element
genericLoad l = load l . fromIntegral

load :: (RAM ll element) => ll -> Address -> element
load l i = indexMaybe l i ?: def

storeNum :: (Read element , Integral element , RAM ll element) => element -> Text -> ll -> Safe ll
storeNum a line l = store' <$> readTextSafe line where store' e = store a e l

storeChar :: (Num element , Integral address , RAM ll element) => address -> Char -> ll -> ll
storeChar a char = genericStore a $ ord char

genericStore :: (Integral value , Num element , Integral address , RAM ll element) => address -> value -> ll -> ll
genericStore a v = store a $ fromIntegral v

store :: (Integral a , RAM ll element) => a -> element -> ll -> ll
store = insertDef . fromIntegral

-- | Types
type Address = Int

type RAM ll element = (Default element , InsertDef ll element, IndexSafe ll element)
