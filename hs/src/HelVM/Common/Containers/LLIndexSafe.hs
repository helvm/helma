{-# LANGUAGE UndecidableInstances #-}
module HelVM.Common.Containers.LLIndexSafe where

import           HelVM.Common.Safe

import           Data.ListLike

import           Prelude           hiding (break, divMod, drop, fromList, length, splitAt, swap, uncons)

-- | Index
naturalIndexSafe :: (MonadSafeError m , IndexSafe full item) => full -> Natural -> m item
naturalIndexSafe l =  indexSafe l . fromIntegral

-- | Type Class
class IndexSafe full item | full -> item where
  findWithDefault :: item -> Int -> full -> item
  findMaybe  :: Int -> full -> Maybe item
  indexMaybe :: full -> Int -> Maybe item
  findSafe   :: MonadSafeError m => Int -> full -> m item
  indexSafe  :: MonadSafeError m => full -> Int -> m item

instance ListLike full item => IndexSafe full item where
  findWithDefault e i = fromMaybe e . findMaybe i
  findMaybe           = flip indexMaybe
  indexMaybe      l   = rightToMaybe . indexSafe l
  findSafe            = flip indexSafe
  indexSafe           = indexSafeLL

-- | Internal functions
indexSafeLL :: (MonadSafeError m , ListLike full item) => full -> Int -> m item
indexSafeLL l i
  | i < 0         = liftError "LLIndexSafe.indexSafeLL: index must be >= 0"
  | length l <= i = liftError "LLIndexSafe.indexSafeLL: index must not found"
  | otherwise     = (pure . index l) i
