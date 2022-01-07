module HelVM.HelMA.Automaton.Memories.Stack.MTStack (
  divMod,
  sub,
  binaryOp,
  binaryOps,
  stackOp,
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

import           HelVM.HelMA.Automaton.BIntegral
--import           HelVM.HelMA.Automaton.Dynamic.DynamicIntegral
import           HelVM.HelMA.Automaton.Operator.BinaryOperator
import           HelVM.HelMA.Automaton.Operator.StackOperator

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.SequencesExtra

import           Data.MonoTraversable
import           Data.Sequences

import           Prelude                                       hiding (divMod, drop, fromList, splitAt, swap)

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

stackOp :: (MonadSafe m, Stack ll) => StackOperator -> ll -> m ll
stackOp (Liter i) = liter i
stackOp (Copy  i) = copy  (fromIntegral i)
stackOp (Move  i) = move  (fromIntegral i)
stackOp (Slide i) = slide (fromIntegral i)
stackOp  Halibut  = halibut
stackOp  Dup      = dup
stackOp  DCopy    = dCopy
stackOp  Rot      = rot
stackOp  Swap     = swap
stackOp  DMove    = dMove
stackOp  Discard  = discard

liter :: (MonadSafe m , Stack ll) => Integer -> ll -> m ll
liter i l = pure $ genericPush1 i l

slide :: (MonadSafe m , IsSequence ll) => Index ll -> ll -> m ll
slide i l = slide' <$> pop1 l where
  slide' (e , l') = (push1 e . drop i) l'

halibut :: (MonadSafe m , Stack ll) => ll -> m ll
halibut l = halibut' =<< pop1 l where
  halibut' (e , l')
    | i <= 0    = flip copy l' $ negate i
    | otherwise = move i l'
      where i = fromIntegral e

-- | Move instructions

rot :: (MonadSafe m , Stack ll) => ll -> m ll
rot = move 2

swap :: (MonadSafe m , Stack ll) => ll -> m ll
swap = move 1

dMove :: (MonadSafe m , Stack ll) => ll -> m ll
dMove l = dMove' =<< pop1 l where
    dMove' (e , l') = move (fromIntegral e) l'

move :: (MonadSafe m , Stack ll) => Index ll -> ll -> m ll
move i l = pure $ l1 <> l2 <> l3 where
  (l1 , l3) = splitAt 1 l'
  (l2 , l') = splitAt i l

-- | Cove instructions

dup :: (MonadSafe m , Stack ll) => ll -> m ll
dup = copy 0

dCopy :: (MonadSafe m , Stack ll) => ll -> m ll
dCopy l = dCopy' =<< pop1 l where
    dCopy' (e , l') = copy (fromIntegral e) l'

copy :: (MonadSafe m , Stack ll) => Index ll -> ll -> m ll
copy i l = flipPush1 l <$> l `indexSafe` i

-- | Push instructions
flipPush1 :: IsSequence ll => ll -> Element ll -> ll
flipPush1 = flip push1

charPush1 :: Stack ll => Char -> ll -> ll
charPush1 = genericPush1 . ord

genericPush1 :: (BIntegral v , Stack ll) => v -> ll -> ll
genericPush1 = push1 . fromIntegral

push1 :: IsSequence ll => Element ll -> ll -> ll
push1 = cons

push2 :: IsSequence ll => Element ll -> Element ll -> ll -> ll
push2 e e' = push1 e . push1 e'

pushList :: IsSequence ll => [Element ll] -> ll -> ll
pushList es l = fromList es <> l

----

type Stack ll = (IsSequence ll , ShowStack ll , BIntegral (Element ll))
type ShowStack ll = (Show ll , Show (Index ll))

----

pop1 :: (MonadSafe m , IsSequence seq) => seq -> m (Element seq , seq)
pop1 = unconsSafe

pop2 :: (MonadSafe m , IsSequence seq) => seq -> m (Element seq , Element seq , seq)
pop2 = uncons2Safe
