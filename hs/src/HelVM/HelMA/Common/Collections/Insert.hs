module HelVM.HelMA.Common.Collections.Insert where

import Data.Default
import Data.Sequence ((|>))

import qualified Data.IntMap     as IntMap
import qualified Data.Sequence   as Seq

class Insert e c | c -> e where
  insert :: Int -> e -> c -> c

instance Default e => Insert e [e] where
  insert 0 e []     = [e]
  insert 0 e (_:xs) = e   : xs
  insert i e []     = def : insert (i-1) e []
  insert i e (x:xs) = x   : insert (i-1) e xs

instance Default e => Insert e (Seq e) where
  insert i e c = insert' $ Seq.length c where
    insert' l
      | i < l       = Seq.update i e c
      | otherwise   = c <> Seq.replicate (i - l) def |> e

instance Insert e (IntMap e) where
  insert = IntMap.insert
