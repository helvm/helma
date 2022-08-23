module HelVM.Common.Collections.MapList where

import           HelVM.Common.Containers.LLIndexSafe
import           HelVM.Common.Containers.LLInsertDef
import           HelVM.Common.Control.Safe

import           Data.Default

import qualified Data.IntMap                         as IntMap
import qualified Data.List.Index                     as List
import qualified Data.ListLike                       as LL
import qualified GHC.Exts                            as I (IsList (..))
import qualified Text.Show

-- | Construction
mapListEmpty :: MapList a
mapListEmpty = mapListFromList []

mapListFromList :: [a] -> MapList a
mapListFromList = fromIndexedList . List.indexed

fromIndexedList :: IndexedList a -> MapList a
fromIndexedList = fromIntMap . IntMap.fromList

fromIntMap :: IntMap a -> MapList a
fromIntMap = MapList

-- | DeConstruction
mapListToList :: Default a => MapList a -> [a]
mapListToList = listFromDescList . toDescList

toDescList :: MapList a -> IndexedList a
toDescList = IntMap.toDescList . unMapList

-- | Internal function
listFromDescList :: Default a => IndexedList a -> [a]
listFromDescList = flip go [] where
  go :: Default a => IndexedList a -> [a] -> [a]
  go []                          acc = acc
  go [(i , v)]                   acc = consDef i $ v : acc
  go ((i1 , v1) : (i2 , v2) : l) acc = go ((i2 , v2) : l) $ consDef (i1 - i2 - 1)  $ v1 : acc

consDef :: Default a => Key -> [a] -> [a]
consDef i l = (check . compare i) 0 where
  check LT = error "MapList.consDef index is negative"
  check EQ = l
  check GT = consDef (i - 1) (def : l)

-- | Types
type Key = IntMap.Key
type IndexedList a = [(Key , a)]
type MapString = MapList Char

newtype MapList a = MapList {unMapList :: IntMap a}
  deriving stock (Eq , Ord, Read)
  deriving newtype (Semigroup , Monoid)

-- | Standard instances
instance (Default a , Show a) => Show (MapList a) where
  show = show . I.toList

instance IsString MapString where
  fromString = mapListFromList

instance Default a => IsList (MapList a) where
  type (Item (MapList a)) = a
  toList      = mapListToList
  fromList    = mapListFromList
  fromListN n = mapListFromList . fromListN n

-- | ListLike instances
instance Default a => LL.FoldableLL (MapList a) a where
  foldl f b = IntMap.foldl f b . unMapList
  foldr f b = IntMap.foldr f b . unMapList

-- | My instances
instance {-# OVERLAPPING #-} IndexSafe (MapList a) a where
  findWithDefault e i = IntMap.findWithDefault e i . unMapList
  findMaybe    = mapListFindMaybe
  indexMaybe   = mapListIndexMaybe
  findSafe   i = liftMaybeOrError "MapList.findSafe: index is not correct" . mapListFindMaybe i
  indexSafe  l = liftMaybeOrError "MapList.LLIndexSafe: index is not correct" . mapListIndexMaybe l

instance InsertDef (MapList a) a where
  insertDef i e = fromIntMap . IntMap.insert i e . unMapList

-- | Internal functions
mapListFindMaybe :: Key -> MapList a -> Maybe a
mapListFindMaybe  i   = IntMap.lookup i . unMapList

mapListIndexMaybe :: MapList a -> Key -> Maybe a
mapListIndexMaybe l i = unMapList l IntMap.!? i
