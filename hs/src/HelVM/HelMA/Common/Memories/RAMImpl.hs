{-#LANGUAGE UndecidableInstances#-}
module HelVM.HelMA.Common.Memories.RAMImpl (
  genericLoad,
  load,
  storeChar,
  genericStore,
  store,
  fromList,
  empty,
  RAM
) where

import Data.Default
import Prelude      hiding (empty , fromList , splitAt)

import qualified HelVM.HelMA.Common.Collections.FromList as I
import qualified HelVM.HelMA.Common.Collections.Insert   as I
import qualified HelVM.HelMA.Common.Collections.Lookup   as I

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

class Default e => RAM e c | c -> e where
  fromList   :: [e] -> c
  empty      :: c
  indexMaybe :: c -> Address -> Maybe e
  insert     :: Address -> e -> c -> c

instance (Default e , I.FromList e c , I.Insert e c , I.Lookup e c) => RAM e c where
  fromList   = I.fromList
  empty      = I.empty
  indexMaybe = I.indexMaybe
  insert     = I.insert
