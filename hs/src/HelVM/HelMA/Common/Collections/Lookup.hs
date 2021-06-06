module HelVM.HelMA.Common.Collections.Lookup where

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

index :: (Show c , Lookup e c) => c -> Int -> e
index c i = check (c `indexMaybe` i) where
  check (Just e) = e
  check  Nothing = error $ "Empty stack " <> show c <> " index " <> show i

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
