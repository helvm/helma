module HelVM.HelMA.Common.Memories.RAM (
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
import Data.List.Index
import Data.Sequence          ((|>))
import Prelude         hiding (empty , fromList , splitAt)

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

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

instance Default e => RAM e [e] where
  fromList   = id
  empty      = []
  indexMaybe = (!!?)
  insert 0 e []     = [e]
  insert 0 e (_:xs) = e   : xs
  insert a e []     = def : insert (a-1) e []
  insert a e (x:xs) = x   : insert (a-1) e xs

instance Default e => RAM e (Seq e) where
  fromList   = Seq.fromList
  empty      = Seq.empty
  indexMaybe = (Seq.!?)
  insert a e c = insert' $ Seq.length c where
    insert' l
      | a < l     = Seq.update a e c
      | otherwise = c <> Seq.replicate (a - l) def |> e

instance Default e => RAM e (IntMap e) where
  fromList   = intMapFromList
  empty      = IntMap.empty
  indexMaybe = (IntMap.!?)
  insert     = IntMap.insert

intMapFromList :: [e] -> IntMap e
intMapFromList = IntMap.fromList . indexed
