module HelVM.HelMA.Common.Memories.StackUtil where

import HelVM.HelMA.Common.BinaryOperator

import HelVM.HelMA.Common.Collections.Drop
import HelVM.HelMA.Common.Collections.FromList
import HelVM.HelMA.Common.Collections.Lookup
import HelVM.HelMA.Common.Collections.Pop
import HelVM.HelMA.Common.Collections.SplitAt

import Prelude hiding (divMod , drop , empty , fromList , splitAt , swap)

type Index = Int

-- Arithmetic

divMod :: (Integral e , Semigroup c , FromList e c , Pop2 e c) => c -> c
divMod = binaryOps [Mod , Div]

sub :: (Integral e , Semigroup c , FromList e c , Pop2 e c) => c -> c
sub = binaryOp Sub

binaryOp :: (Integral e , Semigroup c , FromList e c , Pop2 e c) => BinaryOperator -> c -> c
binaryOp op = binaryOps [op]

binaryOps :: (Integral e , Semigroup c , FromList e c , Pop2 e c) => [BinaryOperator] -> c -> c
binaryOps ops c = pushList (calculateOps e e' ops) c' where (e , e', c') = pop2 c

-- Stack instructions

halibut :: (Show c , Semigroup c , Integral e , FromList e c , Lookup e c , SplitAt e c , Pop1 e c) => c -> c
halibut c
  | i <= 0    = copy (negate i) c'
  | otherwise = move i c'
    where
      i = fromIntegral e
      (e , c') = pop1 c

move :: (Semigroup c , SplitAt e c) => Index -> c -> c
move i c = c1 <> c2 <> c3 where
  (c1 , c3) = splitAt 1 c'
  (c2 , c') = splitAt i c

swap :: (Semigroup c , FromList e c , Pop2 e c) => c -> c
swap c = push2 e' e c' where (e , e' , c') = pop2 c

discard :: Drop e c => c -> c
discard = drop 1

slide :: (Semigroup c , Drop e c , FromList e c , Pop1 e c) => Index -> c -> c
slide i c = push1 e (drop i c') where (e , c') = pop1 c

dup :: (Show c , Semigroup c , FromList e c , Lookup e c) => c -> c
dup = copy 0

copy :: (Show c , Semigroup c , FromList e c , Lookup e c) => Index -> c -> c
copy i c = push1 (c `index` i) c

-- Push instructions

pushChar1 :: (Num e , Semigroup c , FromList e c) => Char -> c -> c
pushChar1 = genericPush1 . ord

genericPush1 :: (Integral v , Num e , Semigroup c , FromList e c) => v -> c -> c
genericPush1 = push1 . fromIntegral

push1 :: (Semigroup c , FromList e c) => e -> c -> c
push1 e = pushList [e]

push2 :: (Semigroup c , FromList e c) => e -> e -> c -> c
push2 e e' = pushList [e , e']

pushList :: (Semigroup c , FromList e c) => [e] -> c -> c
pushList es c = fromList es <> c
