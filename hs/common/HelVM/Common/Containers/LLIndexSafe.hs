{-# LANGUAGE UndecidableInstances #-}
module HelVM.Common.Containers.LLIndexSafe where

import           HelVM.Common.Control.Safe

import           Data.ListLike             hiding (show)

import           Prelude                   hiding (break, divMod, drop, fromList, length, splitAt, swap, uncons)

-- | Index
naturalIndexSafe :: (MonadSafe m , IndexSafe full item) => full -> Natural -> m item
naturalIndexSafe l =  indexSafe l . fromIntegral

-- | Type Class
class IndexSafe full item | full -> item where
  findWithDefault :: item -> Int -> full -> item
  findMaybe  :: Int -> full -> Maybe item
  indexMaybe :: full -> Int -> Maybe item
  findSafe   :: MonadSafe m => Int -> full -> m item
  indexSafe  :: MonadSafe m => full -> Int -> m item

instance ListLike full item => IndexSafe full item where
  findWithDefault e i = fromMaybe e . findMaybe i
  findMaybe           = flip indexMaybe
  indexMaybe      l   = rightToMaybe . indexSafe l
  findSafe            = flip indexSafe
  indexSafe           = indexSafeLL

-- | Internal functions
indexSafeLL :: (MonadSafe m , ListLike full item) => full -> Int -> m item
indexSafeLL l i
  | i < 0     = liftErrorWithTupleList "LLIndexSafe.indexSafeLL: index must be >= 0" [("i" , show i)]
  | ll <= i   = liftErrorWithTupleList "LLIndexSafe.indexSafeLL: index must not found" [("i" , show i) , ("length l" , show ll)]
  | otherwise = (pure . index l) i
    where ll = length l
