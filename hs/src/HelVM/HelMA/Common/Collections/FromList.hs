module HelVM.HelMA.Common.Collections.FromList where

import Prelude hiding (fromList)

import qualified Data.List.Index as List
import qualified Data.IntMap     as IntMap
import qualified Data.Sequence   as Seq

intMapFromList :: [e] -> IntMap e
intMapFromList = IntMap.fromList . List.indexed

class FromList e c | c -> e where
  fromList :: [e] -> c
  empty :: c
  empty = fromList []

instance FromList e [e] where
  fromList = id
  empty = []

instance FromList e (Seq e) where
  fromList = Seq.fromList
  empty = Seq.empty

instance FromList e (IntMap e) where
  fromList = intMapFromList
  empty = IntMap.empty
