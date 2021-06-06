module HelVM.HelMA.Common.Memories.RAMUtil (
  genericLoad,
  load,
  storeChar,
  genericStore,
  store
) where

import HelVM.HelMA.Common.Collections.Insert
import HelVM.HelMA.Common.Collections.Lookup

import Data.Default
import Prelude      hiding (empty , fromList , splitAt)

genericLoad :: (Integral i , Default e , Lookup e c) => c -> i -> e
genericLoad c i = load c $ fromIntegral i

load :: (Default e , Lookup e c) => c -> Int -> e
load c i = indexMaybe c i ?: def

storeChar :: (Num e , Integral a , Insert e c) => a -> Char -> c -> c
storeChar a v = genericStore a $ ord v

genericStore :: (Integral v , Num e , Integral a , Insert e c) => a -> v -> c -> c
genericStore a v = store a $ fromIntegral v

store :: (Integral a , Insert e c) => a -> e -> c -> c
store = insert . fromIntegral
