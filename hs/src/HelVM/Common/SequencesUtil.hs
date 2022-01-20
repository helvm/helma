module HelVM.Common.SequencesUtil where

import           HelVM.Common.Control.Safe

import           Control.Type.Operator

import           Data.Default
import           Data.MonoTraversable
import           Data.Sequence             ((|>))
import           Data.Sequences

import           Prelude                   hiding (break, divMod, drop, fromList, length, splitAt, swap, uncons)

import qualified Data.Sequence             as Seq

-- | Construct a sequence
maybeToFromList :: (MonoPointed p, Monoid p) => Maybe (Element p) -> p
maybeToFromList (Just e) = singleton e
maybeToFromList Nothing  = mempty

-- | Index
naturalIndexSafe :: (MonadSafe m , Show seq , Show (Index seq) , IsSequence seq) => seq -> Natural -> m (Element seq)
naturalIndexSafe c = indexSafe c . fromIntegral

indexSafe :: (MonadSafe m , Show seq, Show (Index seq), IsSequence seq) => seq -> Index seq -> m (Element seq)
indexSafe c i = (liftMaybeOrErrorTupleList [("Lookup.LLIndexSafe" , show c) , ("index" , show i)] . index c) i

lookup :: (MonadSafe m , Show seq, Show (Index seq), IsSequence seq) => Index seq -> seq -> m (Element seq)
lookup = flip indexSafe

-- | Split a sequence
splitBy :: (Eq (Element seq) , IsSequence seq) => Element seq -> seq -> (seq , seq)
splitBy separator l =  (acc , drop 1 l') where (acc , l') = break (== separator) l

-- | Pop
discard :: (MonadSafe m , IsSequence seq) => seq -> m seq
discard s = snd <$> unconsSafe s

top :: (MonadSafe m , IsSequence seq) => seq -> m $ Element seq
top s = fst <$> unconsSafe s

unconsSafe :: (MonadSafe m , IsSequence seq) => seq -> m (Element seq , seq)
unconsSafe = liftMaybeOrError "Empty IsSequence" . uncons

uncons2Safe :: (MonadSafe m , IsSequence seq) => seq -> m (Element seq , Element seq , seq)
uncons2Safe = liftMaybeOrError "Empty" . uncons2

uncons2 :: IsSequence seq => seq -> Maybe (Element seq, Element seq, seq)
uncons2 s = uncons2' =<< uncons s where
  uncons2' (e , s') = uncons2'' <$> uncons s' where
    uncons2'' (e' , s'') = (e , e' , s'')

-- | Insert a new element
class (Integral (Index seq) , SemiSequence seq) => InsertDef seq where
  insertDef :: Index seq -> Element seq -> seq -> seq

instance Default a => InsertDef [a] where
  insertDef 0 e []       = [e]
  insertDef 0 e (_ : xs) = e   : xs
  insertDef i e []       = def : insertDef (i-1) e []
  insertDef i e (x : xs) = x   : insertDef (i-1) e xs

instance Default a => InsertDef (Seq a) where
  insertDef i e c = (check . Seq.length) c where
    check l
      | i < l       = Seq.update i e c
      | otherwise   = c <> Seq.replicate (i - l) def |> e
