{-#LANGUAGE ConstraintKinds#-}
module HelVM.HelMA.Automaton.Memories.RAMConst (
  genericLoad,
  load,
  storeNum,
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

import HelVM.Common.Containers.FromList
import HelVM.Common.Containers.Insert
import HelVM.Common.Containers.Lookup

import HelVM.Common.ReadText
import HelVM.Common.Safe

type Address = Int

genericLoad :: (Integral i , RAM e c) => c -> i -> e
genericLoad c i = load c $ fromIntegral i

load :: (RAM e c) => c -> Address -> e
load c i = indexMaybe c i ?: def

storeNum :: (Read e , Integral e , RAM e c) => e -> Text -> c -> Safe c
storeNum a line c = store' <$> readTextSafe line where store' e = store a e c

storeChar :: (Num e , Integral a , RAM e c) => a -> Char -> c -> c
storeChar a char = genericStore a $ ord char

genericStore :: (Integral v , Num e , Integral a , RAM e c) => a -> v -> c -> c
genericStore a value = store a $ fromIntegral value

store :: (Integral a , RAM e c) => a -> e -> c -> c
store = insert . fromIntegral

type RAM e c = (Default e , FromList e c , Insert e c , Lookup e c)
