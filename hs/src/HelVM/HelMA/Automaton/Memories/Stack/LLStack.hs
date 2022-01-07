module HelVM.HelMA.Automaton.Memories.Stack.LLStack (
  divMod,
  sub,
  binaryOp,
  binaryOps,
  halibut,
  stackOp,
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

import           HelVM.HelMA.Automaton.BIntegral
import           HelVM.HelMA.Automaton.Operator.BinaryOperator
import           HelVM.HelMA.Automaton.Operator.StackOperator

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ListLikeExtra

import           Data.ListLike
import           Prelude                                       hiding (divMod, drop, fromList, splitAt, swap)

-- | Arithmetic
divMod :: (MonadSafe m , BIntegral element , Stack ll element) => ll -> m ll
divMod = binaryOps [Mod , Div]

sub :: (MonadSafe m , BIntegral element , Stack ll element) => ll -> m ll
sub = binaryOp Sub

binaryOp :: (MonadSafe m , BIntegral element , Stack ll element) => BinaryOperator -> ll -> m ll
binaryOp op = binaryOps [op]

binaryOps :: (MonadSafe m , BIntegral element , Stack ll element) => [BinaryOperator] -> ll -> m ll
binaryOps ops l = binaryOps' <$> pop2 l where
  binaryOps' (e , e', l') = pushList (calculateOps e e' ops) l'

-- | Stack instructions

stackOp :: (MonadSafe m , BIntegral element , Stack ll element) => StackOperator -> ll -> m ll
stackOp (Liter i) = liter i
stackOp (Copy  i) = copy i
stackOp (Move  i) = move i
stackOp (Slide i) = slide i
stackOp  Halibut  = halibut
stackOp  Dup      = dup
stackOp  DCopy    = dCopy
stackOp  Rot      = rot
stackOp  Swap     = swap
stackOp  DMove    = dMove
stackOp  Discard  = discard

liter :: (MonadSafe m , BIntegral element, Stack ll element) => Integer -> ll -> m ll
liter i l = pure $ genericPush1 i l

slide :: (MonadSafe m , Stack ll element) => Index -> ll -> m ll
slide i l = slide' <$> pop1 l where
  slide' (e , l') = push1 e $ drop i l'

halibut :: (MonadSafe m , BIntegral element , Stack ll element) => ll -> m ll
halibut l = halibut' =<< pop1 l where
  halibut' (e , l')
    | i <= 0    = copy (negate i) l'
    | otherwise = move i l'
      where i = fromIntegral e

-- | Move instructions

rot :: (MonadSafe m , Stack ll element) => ll -> m ll
rot = move 2

swap :: (MonadSafe m , Stack ll element) => ll -> m ll
swap = move 1

dMove :: (MonadSafe m , Stack ll element , Integral element) => ll -> m ll
dMove l = dMove' =<< pop1 l where
    dMove' (e , l') = move (fromIntegral e) l'

move :: (MonadSafe m , Stack ll element) => Index -> ll -> m ll
move i l = pure $ l1 <> l2 <> l3 where
  (l1 , l3) = splitAt 1 l'
  (l2 , l') = splitAt i l

-- | Copy instructions

dup :: (MonadSafe m , Stack ll element) => ll -> m ll
dup = copy 0

dCopy :: (MonadSafe m , Stack ll element , Integral element) => ll -> m ll
dCopy l = dCopy' =<< pop1 l where
    dCopy' (e , l') = copy (fromIntegral e) l'

copy :: (MonadSafe m , Stack ll element) => Index -> ll -> m ll
copy i l = flipPush1 l <$> l `indexSafe` i

-- | Push instructions
flipPush1 :: Stack ll element => ll -> element -> ll
flipPush1 = flip push1

charPush1 :: (Num element , Stack ll element) => Char -> ll -> ll
charPush1 = genericPush1 . ord

genericPush1 :: (BIntegral v , Num element , Stack ll element) => v -> ll -> ll
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

pop1 :: (MonadSafe m , ListLike full item) => full -> m (item , full)
pop1 = unconsSafe

pop2 :: (MonadSafe m , ListLike full item) => full -> m (item , item , full)
pop2 = uncons2Safe
