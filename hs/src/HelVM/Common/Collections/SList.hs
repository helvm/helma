{-# LANGUAGE DeriveTraversable #-}
module HelVM.Common.Collections.SList where

import           HelVM.Common.Containers.LLInsertDef as LL
import           HelVM.Common.Containers.MTInsertDef as MT

import           Prelude                             hiding (reverse, uncons)

import           Control.Type.Operator
import           Data.Default

import qualified Data.Foldable                       as F
import qualified Data.ListLike                       as LL
import qualified Data.MonoTraversable                as MT
import qualified Data.Sequences                      as S
import qualified GHC.Exts                            as I (IsList (..))
import qualified Slist                               as L
import qualified Text.Show

-- | Public functions
chunksOf :: Int -> SList a -> SList $ SList a
chunksOf i sl = SList $ SList <$> L.chunksOf i (unSList sl)

-- | Construction
sListEmpty :: SList a
sListEmpty = SList mempty

sListFromList :: [a] -> SList a
sListFromList = SList . fromList

-- | DeConstruction
sListToList :: SList a -> [a]
sListToList = toList . unSList

-- | Types
type SString  = SList Char

newtype SList a = SList { unSList :: L.Slist a}
  deriving stock (Eq , Ord, Read)
  deriving stock (Foldable , Functor , Traversable)
  deriving newtype (Semigroup , Monoid , Applicative , Monad)

-- | Standard instances
instance Show a => Show (SList a) where
  show = show . toList

instance IsString SString where
  fromString = SList . L.slist

instance IsList (SList a) where
  type (Item (SList a)) = a
  toList      = sListToList
  fromList    = sListFromList
  fromListN n = SList . fromListN n

-- | MonoFoldable instances
type instance MT.Element (SList a) = a

instance MT.MonoFoldable (SList a) where
  headEx = sListHead
  lastEx = sListLast

-- | SemiSequence instances
instance MT.GrowingAppend (SList a) where

instance S.SemiSequence (SList a) where
  type Index (SList a) = Int
  cons          = sListCons
  snoc          = sListSnoc
  reverse       = sListReverse
  sortBy        = sListSortBy
  intersperse   = sListIntersperse
  find          = sListFind

-- | IsSequence instances
instance MT.MonoPointed (SList a) where

instance MT.MonoFunctor (SList a) where

instance MT.MonoTraversable (SList a) where

instance S.IsSequence (SList a) where
  tailEx        = sListTail
  initEx        = sListTail
  replicate     = sListReplicate
  uncons        = sListUncons

-- | ListLike instances
instance LL.FoldableLL (SList a) a where
--  foldl  = F.foldl
  foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
  foldr  = F.foldr
  foldl1 = F.foldl1
  foldr1 = F.foldr1
  foldl' = F.foldl'
  foldr' = F.foldr'

instance LL.ListLike (SList a) a where
  empty         = mempty
  singleton     = pure
  cons          = sListCons
  snoc          = sListSnoc
  append        = (<>)
  head          = sListHead
  uncons        = sListUncons
  last          = sListLast
  tail          = sListTail
  init          = sListInit
  null          = L.isEmpty . unSList
--  length = genericLength
--  map           = fmap
  rigidMap = fmap
  reverse       = sListReverse
  intersperse   = sListIntersperse
--  concat = fold
--  concatMap = foldMap
--  rigidConcatMap = concatMap
--  any p = getAny . foldMap (Any . p)
--  all p = getAll . foldMap (All . p)
--  maximum = foldr1 max
--  minimum = foldr1 min
  replicate     = sListReplicate
--  take = genericTake
--  drop = genericDrop
--  splitAt = genericSplitAt
--  takeWhile
--  dropWhile
--  dropWhileEnd func = foldr (\x xs -> if func x && null xs then empty else cons x xs) empty
--  span
--  break p = span (not . p)
--  group = groupBy (==)
--  inits
--  tails
--  isPrefixOf
--  isSuffixOf needle haystack = isPrefixOf (reverse needle) (reverse haystack)
--  isInfixOf
--  stripPrefix
--  stripSuffix
--  elem i = any (== i)
--  notElem i = all (/= i)
  find          = sListFind
--  filter
--  partition p xs = (filter p xs, filter (not . p) xs)
  index         = sListIndex
--  elemIndex e l = findIndex (== e) l
--  elemIndices i l = findIndices (== i) l
--  findIndex f = listToMaybe . findIndices f
--  findIndices
--  sequence
--  mapM
--  rigidMapM = mapM
--  nub = nubBy (==)
--  delete = deleteBy (==)
--  deleteFirsts = foldl (flip delete)
--  union = unionBy (==)
--  intersect = intersectBy (==)
--  sort = sortBy compare
--  insert = insertBy compare
--  toList'      = sListToList
--  fromList'    = sListFromList
--  fromListLike = map id
--  nubBy
--  deleteBy
--  deleteFirstsBy func = foldl (flip (deleteBy func))
--  unionBy func x y = append x $ foldl (flip (deleteBy func)) (nubBy func y) x
--  intersectBy func xs ys = filter (\x -> any (func x) ys) xs
--  groupBy
  sortBy        = sListSortBy
--  insertBy
  genericLength = L.genericLength . unSList
--  genericTake
--  genericDrop
--  genericSplitAt n l = (genericTake n l, genericDrop n l)
--  genericReplicate

-- | My instances
instance Default a => MT.InsertDef (SList a) where
  insertDef i e = sListFromList. MT.insertDef i e . sListToList

instance Default a => LL.InsertDef (SList a) a where
  insertDef i e = sListFromList. LL.insertDef i e . sListToList

-- | Internals sList
sListCons :: a -> SList a -> SList a
sListCons e = SList . L.cons e . unSList

sListSnoc :: LL.ListLike a (I.Item a) => a -> I.Item a -> a
sListSnoc l e = l <> LL.singleton e

sListHead :: SList a -> a
sListHead = L.head . unSList

sListUncons :: SList a -> Maybe (a, SList a)
sListUncons l = wrap <$> (L.uncons . unSList) l where
  wrap :: (a , L.Slist a) -> (a , SList a)
  wrap (a , l') = (a , SList l')

sListLast :: SList a -> a
sListLast = L.last . unSList

sListTail :: SList a -> SList a
sListTail = SList . L.tail . unSList

sListInit :: SList a -> SList a
sListInit = SList . L.init . unSList

sListReverse :: SList a -> SList a
sListReverse = SList . L.reverse . unSList

sListIntersperse :: a -> SList a -> SList a
sListIntersperse e = SList . L.intersperse e . unSList

sListReplicate :: Int -> a -> SList a
sListReplicate e = SList . L.replicate e

sListFind :: (a -> Bool) -> SList a -> Maybe a
sListFind e = find e . sListToList

sListIndex :: SList a -> Int -> a
sListIndex = flip sListUnsafeAt

sListUnsafeAt :: Int -> SList a -> a
sListUnsafeAt i = L.unsafeAt i . unSList

sListSortBy :: (a -> a -> Ordering) -> SList a -> SList a
sListSortBy f = SList . L.sortBy f . unSList
