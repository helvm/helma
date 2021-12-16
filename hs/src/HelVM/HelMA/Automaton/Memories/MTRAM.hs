module HelVM.HelMA.Automaton.Memories.MTRAM (
  genericLoad,
  load,
  storeNum,
  storeChar,
  genericStore,
  store,
  fromList,
  RAM,
) where

import           HelVM.Common.Containers.MTIndexSafe
import           HelVM.Common.Containers.MTInsertDef
import           HelVM.Common.Control.Safe
import           HelVM.Common.ReadText

import           Data.Default
import           Data.MonoTraversable
import           Data.Sequences
import           Prelude                             hiding (divMod, drop, fromList, splitAt, swap)

genericLoad :: ((Default (Element ll)) , (Num (Index ll)) , Integral index , RAM ll) => ll -> index -> Element ll
genericLoad l = load l . fromIntegral

load :: ((Default (Element ll)) , RAM ll) => ll -> Index ll -> Element ll
load l i = indexMaybe l i ?: def

storeNum :: ((Read (Element ll)) , (Num (Index ll)), (Integral (Element ll)) , RAM ll) => Element ll -> Text -> ll -> Safe ll
storeNum a line l = store' <$> readTextSafe line where store' e = store a e l

storeChar :: ((Num (Index ll)) , (Num (Element ll)) , Integral a , RAM ll) => a -> Char -> ll -> ll
storeChar a char = genericStore a $ ord char

genericStore :: ( (Num (Element ll)) , Integral value , (Num (Index ll)) , Integral address , RAM ll) => address -> value -> ll -> ll
genericStore a = store a . fromIntegral

store :: ((Num (Index ll)), Integral a , RAM ll) => a -> Element ll -> ll -> ll
store = insertDef . fromIntegral

-- | Types
type RAM ll = (InsertDef ll, IndexSafe ll)
