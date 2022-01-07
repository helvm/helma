module HelVM.HelMA.Automaton.Memories.RAM.MTRAM (
  genericLoad,
  load,
  storeNum,
  storeChar,
  genericStore,
  store,
  fromList,
  RAM,
) where

import           HelVM.HelMA.Automaton.BIntegral

import           HelVM.HelIO.Containers.MTIndexSafe
import           HelVM.HelIO.Containers.MTInsertDef
import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ReadText

import           Data.Default
import           Data.MonoTraversable
import           Data.Sequences
import           Prelude                            hiding (divMod, drop, fromList, splitAt, swap)

genericLoad :: ((Default (Element ll)) , (Num (Index ll)) , BIntegral index , RAM ll) => ll -> index -> Element ll
genericLoad l = load l . fromIntegral

load :: ((Default (Element ll)) , RAM ll) => ll -> Index ll -> Element ll
load l i = indexMaybe l i ?: def

storeNum :: ((Num (Index ll)), (BIntegral (Element ll)) , RAM ll) => Element ll -> Text -> ll -> Safe ll
storeNum a line l = store' <$> readTextSafe line where store' e = store a e l

storeChar :: ((Num (Index ll)) , (Num (Element ll)) , BIntegral a , RAM ll) => a -> Char -> ll -> ll
storeChar a char = genericStore a $ ord char

genericStore :: ( (Num (Element ll)) , BIntegral value , (Num (Index ll)) , BIntegral address , RAM ll) => address -> value -> ll -> ll
genericStore a = store a . fromIntegral

store :: ((Num (Index ll)), BIntegral a , RAM ll) => a -> Element ll -> ll -> ll
store = insertDef . fromIntegral

-- | Types
type RAM ll = (InsertDef ll, IndexSafe ll)
