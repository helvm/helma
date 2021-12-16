module HelVM.HelMA.Automaton.Memories.LLStack (
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
  splitAt,
  drop,
  pop1,
  pop2,
  Stack,
) where

import           HelVM.HelMA.Automaton.BinaryOperator

import           HelVM.Common.Containers.LLIndexSafe
import           HelVM.Common.Control.Safe
import           HelVM.Common.ListLikeUtil

import           Data.ListLike
import           Prelude                              hiding (divMod, drop, fromList, splitAt, swap)

-- | Arithmetic
divMod :: (MonadSafe m , Integral element , Stack ll element) => ll -> m ll
divMod = binaryOps [Mod , Div]

sub :: (MonadSafe m , Integral element , Stack ll element) => ll -> m ll
sub = binaryOp Sub

binaryOp :: (MonadSafe m , Integral element , Stack ll element) => BinaryOperator -> ll -> m ll
binaryOp op = binaryOps [op]

binaryOps :: (MonadSafe m , Integral element , Stack ll element) => [BinaryOperator] -> ll -> m ll
binaryOps ops l = binaryOps' <$> pop2 l where
  binaryOps' (e , e', l') = pushList (calculateOps e e' ops) l'

-- | Stack instructions
halibut :: (MonadSafe m , Integral element , Stack ll element) => ll -> m ll
halibut l = halibut' =<< pop1 l where
  halibut' (e , l')
    | i <= 0    = copy (negate i) l'
    | otherwise = pure $ move i l'
      where i = fromIntegral e

move :: Stack ll element => Index -> ll -> ll
move i l = l1 <> l2 <> l3 where
  (l1 , l3) = splitAt 1 l'
  (l2 , l') = splitAt i l

swap :: (MonadSafe m , Stack ll element) => ll -> m ll
swap l = swap' <$> pop2 l where
  swap' (e , e', l') = push2 e' e l'

slide :: (MonadSafe m , Stack ll element) => Index -> ll -> m ll
slide i l = slide' <$> pop1 l where
  slide' (e , l') = push1 e $ drop i l'

dup :: (MonadSafe m , Stack ll element) => ll -> m ll
dup = copy 0

copy :: (MonadSafe m , Stack ll element) => Index -> ll -> m ll
copy i l = flipPush1 l <$> l `indexSafe` i

-- | Push instructions
flipPush1 :: Stack ll element => ll -> element -> ll
flipPush1 = flip push1

charPush1 :: (Num element , Stack ll element) => Char -> ll -> ll
charPush1 = genericPush1 . ord

genericPush1 :: (Integral v , Num element , Stack ll element) => v -> ll -> ll
genericPush1 = push1 . fromIntegral

push1 :: Stack ll element => element -> ll -> ll
push1 e = pushList [e]

push2 :: Stack ll element => element -> element -> ll -> ll
push2 e e' = pushList [e , e']

pushList :: Stack ll element => [element] -> ll -> ll
pushList es l = fromList es <> l

-- | Types
type Index = Int
type Stack ll element = (Show ll , ListLike ll element , IndexSafe ll element)
