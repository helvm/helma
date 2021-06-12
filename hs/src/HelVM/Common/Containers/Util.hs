module HelVM.Common.Containers.Util where

import HelVM.Common.Safe

import Relude.Extra

indexSafeByKey :: (Show k, Ord k, Show v) => k -> Map k v -> Safe v
indexSafeByKey k mapKV = maybeToSafeOrErrorTupleList [( "key" , show k) , ("map" , show mapKV)] $ lookup k mapKV

showFoldable ::  (Foldable c , Functor c , Show e) => c e -> Text
showFoldable f = fmconcat $ show <$> f

fmconcat ::  (Foldable c , Monoid e) => c e -> e
fmconcat =  mconcat . toList
--fmconcat = foldr mappend mempty
