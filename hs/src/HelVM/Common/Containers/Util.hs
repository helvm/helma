module HelVM.Common.Containers.Util where

import           HelVM.Common.Control.Safe

import           Relude.Extra

indexSafeByKey :: (MonadSafe m , Show k , Ord k , Show v) => k -> Map k v -> m v
indexSafeByKey k mapKV = liftMaybeOrErrorTupleList [("key" , show k) , ("mapKV" , show mapKV)] $ lookup k mapKV

showFoldable ::  (Foldable c , Functor c , Show e) => c e -> Text
showFoldable f = fmconcat $ show <$> f

fmconcat ::  (Foldable c , Monoid e) => c e -> e
fmconcat = mconcat . toList
--fmconcat = foldr mappend mempty
