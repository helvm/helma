module HelVM.HelMA.Automaton.Memories.MTStack (
  divMod,
  sub,
  binaryOp,
  binaryOps,
  halibut,
  move,
  swap,
  discard,
  slide,
  dup,
  copy,
  flipPush1,
  charPush1,
  genericPush1,
  push1,
  push2,
  lookup,
  splitAt,
  drop,
  pop1,
  pop2,
  Stack,
) where

import           HelVM.HelMA.Automaton.BinaryOperator

import           HelVM.Common.Control.Safe
import           HelVM.Common.SequencesUtil

import           Data.MonoTraversable
import           Data.Sequences

import           Prelude                              hiding (divMod, drop, fromList, splitAt, swap)

-- | Arithmetic
divMod :: (MonadSafe m , Stack ll) => ll -> m ll
divMod = binaryOps [Mod , Div]

sub :: (MonadSafe m , Stack ll) => ll -> m ll
sub = binaryOp Sub

binaryOp :: (MonadSafe m , Stack ll) => BinaryOperator -> ll -> m ll
binaryOp op = binaryOps [op]

binaryOps :: (MonadSafe m , Stack ll) => [BinaryOperator] -> ll -> m ll
binaryOps ops l = binaryOps' <$> pop2 l where
  binaryOps' (e , e', l') = flip pushList l' $ calculateOps e e' ops

-- | Stack instructions
halibut :: (MonadSafe m, Stack ll) => ll -> m ll
halibut l = halibut' =<< pop1 l where
  halibut' (e , l')
    | i <= 0    = flip copy l' $ negate i
    | otherwise = (pure . move i) l'
      where i = fromIntegral e

move :: IsSequence ll => Index ll -> ll -> ll
move i l = l1 <> l2 <> l3 where
  (l1 , l3) = splitAt 1 l'
  (l2 , l') = splitAt i l

swap :: (MonadSafe m , IsSequence ll) => ll -> m ll
swap l = swap' <$> pop2 l where
  swap' (e , e', l') = push2 e' e l'

slide :: (MonadSafe m , IsSequence ll) => Index ll -> ll -> m ll
slide i l = slide' <$> pop1 l where
  slide' (e , l') = (push1 e . drop i) l'

dup :: (MonadSafe m , Stack ll) => ll -> m ll
dup = copy 0

copy :: (MonadSafe m , Stack ll) => Index ll -> ll -> m ll
copy i l = flipPush1 l <$> l `indexSafe` i

-- | Push instructions
flipPush1 :: IsSequence ll => ll -> Element ll -> ll
flipPush1 = flip push1

charPush1 :: Stack ll => Char -> ll -> ll
charPush1 = genericPush1 . ord

genericPush1 :: (Integral v , Stack ll) => v -> ll -> ll
genericPush1 = push1 . fromIntegral

push1 :: IsSequence ll => Element ll -> ll -> ll
push1 = cons

push2 :: IsSequence ll => Element ll -> Element ll -> ll -> ll
push2 e e' = push1 e . push1 e'

pushList :: IsSequence ll => [Element ll] -> ll -> ll
pushList es l = fromList es <> l

----

type Stack ll = (IsSequence ll , ShowStack ll , Integral (Element ll))
type ShowStack ll = (Show ll , Show (Index ll))
