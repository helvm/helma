module HelVM.Common.Containers.FromList where

import Prelude hiding (empty , fromList , one)

import qualified Data.List.Index     as List
import qualified Data.List.Singleton as List
import qualified Data.IntMap         as IntMap
import qualified Data.Sequence       as Seq
import qualified Data.Text           as Text

maybeToFromList :: FromList e c => Maybe e -> c
maybeToFromList (Just e) = one e
maybeToFromList Nothing  = empty

intMapFromList :: [e] -> IntMap e
intMapFromList = IntMap.fromList . List.indexed

class FromList e c | c -> e where
  fromList :: [e] -> c
  empty    :: c
  one      :: e -> c
  empty    = fromList []
  one    e = fromList [e]

instance FromList e [e] where
  fromList = id
  empty    = []
  one      = List.singleton

instance FromList e (Seq e) where
  fromList = Seq.fromList
  empty    = Seq.empty
  one      = Seq.singleton

instance FromList e (IntMap e) where
  fromList = intMapFromList
  empty    = IntMap.empty
  one      = IntMap.singleton 0

instance FromList Char Text where
  fromList = toText
  empty    = Text.empty
  one      = Text.singleton
