module HelVM.Common.ListLikeUtil where

import           HelVM.Common.Control.Safe

import           Data.ListLike

import           Prelude                   hiding (break, divMod, drop, fromList, length, splitAt, swap, toList, uncons)

-- | Don't use ListLikeUtil
-- | Use SequencesUtil

-- | Construction
convert :: (ListLike full1 item , ListLike full2 item) => full1 -> full2
convert = fromList . toList

maybeToFromList :: ListLike full item => Maybe item -> full
maybeToFromList (Just e) = singleton e
maybeToFromList Nothing  = mempty

-- | Split
splitBy :: (Eq item , ListLike full item) => item -> full -> (full , full)
splitBy separator l =  (acc , drop 1 l') where (acc , l') = break (== separator) l

-- | Pop
discard :: (MonadSafe m , ListLike full item) => full -> m full
discard l = snd <$> pop1 l

top :: (MonadSafe m , ListLike full item) => full -> m item
top s = fst <$> pop1 s

pop1 :: (MonadSafe m , ListLike full item) => full -> m (item , full)
pop1 = liftMaybeOrError "Empty" . uncons

pop2 :: (MonadSafe m , ListLike full item) => full -> m (item , item , full)
pop2 = liftMaybeOrError "Empty" . uncons2

uncons2 :: ListLike full item => full -> Maybe (item, item, full)
uncons2 l = uncons2' =<< uncons l where
  uncons2' (e , l') = uncons2'' <$> uncons l' where
    uncons2'' (e' , l'') = (e , e' , l'')
