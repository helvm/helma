{-# LANGUAGE UndecidableInstances #-}
module HelVM.Common.Containers.MTIndexSafe where

import           HelVM.Common.Control.Safe

import           Control.Type.Operator
import           Data.MonoTraversable
import           Data.Sequences

import           Prelude                   hiding (break, divMod, drop, fromList, length, splitAt, swap, uncons)

-- | Index
naturalIndexSafe :: (MonadSafe m , IndexSafe seq , Num $ Index seq) => seq -> Natural -> m $ Element seq
naturalIndexSafe l =  indexSafe l . fromIntegral

-- | Type Class
class IndexSafe seq where
  findWithDefault :: Element seq -> Index seq -> seq -> Element seq
  findMaybe  :: Index seq -> seq -> Maybe $ Element seq
  indexMaybe :: seq -> Index seq -> Maybe $ Element seq
  findSafe   :: MonadSafe m => Index seq -> seq -> m $ Element seq
  indexSafe  :: MonadSafe m => seq -> Index seq -> m $ Element seq

instance IsSequence seq => IndexSafe seq where
  findWithDefault e i = fromMaybe e . findMaybe i
  findMaybe           = flip indexMaybe
  indexMaybe      l   = rightToMaybe . indexSafe l
  findSafe            = flip indexSafe
  indexSafe       l   = liftMaybeOrError "MTIndexSafe.indexSafe:" . index l
