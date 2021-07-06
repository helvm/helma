module HelVM.Common.Containers.Lookup where

import HelVM.Common.Safe

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

naturalIndexSafe :: (MonadSafeError m , Show c , Lookup e c) => c -> Natural -> m e
naturalIndexSafe c i = indexSafe c $ fromIntegral i

indexSafe :: (MonadSafeError m , Show c , Lookup e c) => c -> Int -> m e
indexSafe c i = liftMaybeOrErrorTupleList [("Lookup.indexSafe" , show c) , ("index" , show i)] $ indexMaybe c i

indexMaybe :: Lookup e c => c -> Int -> Maybe e
indexMaybe = flip lookup

class Lookup e c | c -> e where
  lookup:: Int -> c -> Maybe e

instance Lookup e [e] where
  lookup = flip (!!?)

instance Lookup e (Seq e) where
  lookup = Seq.lookup

instance Lookup e (IntMap e) where
  lookup = IntMap.lookup
