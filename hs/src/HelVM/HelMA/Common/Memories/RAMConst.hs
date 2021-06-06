{-#LANGUAGE ConstraintKinds#-}
module HelVM.HelMA.Common.Memories.RAMConst (
  genericLoad,
  load,
  storeChar,
  genericStore,
  store,
  fromList,
  empty,
  RAM,
  intMapFromList
) where

import Data.Default
import Prelude      hiding (empty , fromList , splitAt)

import HelVM.HelMA.Common.Collections.FromList
import HelVM.HelMA.Common.Collections.Insert
import HelVM.HelMA.Common.Collections.Lookup

type Address = Int

genericLoad :: (Integral i , RAM e c) => c -> i -> e
genericLoad c i = load c $ fromIntegral i

load :: (RAM e c) => c -> Address -> e
load c i = indexMaybe c i ?: def

storeChar :: (Num e , Integral a , RAM e c) => a -> Char -> c -> c
storeChar a char = genericStore a $ ord char

genericStore :: (Integral v , Num e , Integral a , RAM e c) => a -> v -> c -> c
genericStore a value = store a $ fromIntegral value

store :: (Integral a , RAM e c) => a -> e -> c -> c
store = insert . fromIntegral

type RAM e c = (Default e , FromList e c , Insert e c , Lookup e c)
