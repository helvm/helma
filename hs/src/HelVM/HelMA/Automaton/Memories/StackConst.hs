{-#LANGUAGE ConstraintKinds#-}
module HelVM.HelMA.Automaton.Memories.StackConst (
  Index,
  Stack,
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
  empty,
  lookup,
  splitAt,
  drop,
  pop1,
  pop2
) where

import HelVM.HelMA.Automaton.BinaryOperator

import HelVM.Common.Safe

import Prelude hiding (divMod , drop , empty , fromList , splitAt , swap)

import HelVM.Common.Containers.FromList
import HelVM.Common.Containers.Lookup
import HelVM.Common.Containers.Pop
import HelVM.Common.Containers.SplitAt

type Index = Int

-- Arithmetic

divMod :: (Integral e , Stack e c) => c -> Safe c
divMod = binaryOps [Mod , Div]

sub :: (Integral e , Stack e c) => c -> Safe c
sub = binaryOp Sub

binaryOp :: (Integral e , Stack e c) => BinaryOperator -> c -> Safe c
binaryOp op = binaryOps [op]

binaryOps :: (Integral e , Stack e c) => [BinaryOperator] -> c -> Safe c
binaryOps ops c = binaryOps' <$> pop2 c where
  binaryOps' (e , e', c') = pushList (calculateOps e e' ops) c'

-- Stack instructions

halibut :: (Integral e , Stack e c) => c -> Safe c
halibut c = halibut' =<< pop1 c where
  halibut' (e , c')
    | i <= 0    = copy (negate i) c'
    | otherwise = pure $ move i c'
      where i = fromIntegral e

move :: Stack e c => Index -> c -> c
move i c = c1 <> c2 <> c3 where
  (c1 , c3) = splitAt 1 c'
  (c2 , c') = splitAt i c

swap :: Stack e c => c -> Safe c
swap c = swap' <$> pop2 c where
  swap' (e , e', c') = push2 e' e c'


slide :: Stack e c => Index -> c -> Safe c
slide i c = slide' <$> pop1 c where
  slide' (e , c') = push1 e $ drop i c'

dup :: Stack e c => c -> Safe c
dup = copy 0

copy :: Stack e c => Index -> c -> Safe c
copy i c = flipPush1 c <$> indexSafe c i

-- Push instructions

flipPush1 :: Stack e c => c -> e -> c
flipPush1 = flip push1

charPush1 :: (Num e , Stack e c) => Char -> c -> c
charPush1 = genericPush1 . ord

genericPush1 :: (Integral v , Num e , Stack e c) => v -> c -> c
genericPush1 = push1 . fromIntegral

push1 :: Stack e c => e -> c -> c
push1 e = pushList [e]

push2 :: Stack e c => e -> e -> c -> c
push2 e e' = pushList [e , e']

pushList :: Stack e c => [e] -> c -> c
pushList es c = fromList es <> c

----

type Stack e c = (Show c , Semigroup c , FromList e c , Lookup e c , SplitAt e c , Pop1 e c , Pop2 e c)
