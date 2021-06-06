module HelVM.HelMA.Common.Memories.Stack (
  Index,
  Stack,
  divMod,
  sub,
  binaryOp,
  binaryOps,
  pushChar1,
  genericPush1,
  halibut,
  move,
  swap,
  discard,
  slide,
  dup,
  copy,
  empty,
  lookup,
  splitAt,
  drop,
  push1,
  push2,
  pop1,
  pop2
) where

import HelVM.HelMA.Common.BinaryOperator

import Data.Sequence        (Seq(..))
import Prelude       hiding (divMod , drop , empty , fromList , splitAt , swap)

import qualified Data.Sequence as Seq
import qualified Prelude       as List (drop , splitAt)

type Index = Int

-- Arithmetic

divMod :: (Integral e , Stack e c) => c -> c
divMod = binaryOps [Mod , Div]

sub :: (Integral e , Stack e c) => c -> c
sub = binaryOp Sub

binaryOp :: (Integral e , Stack e c) => BinaryOperator -> c -> c
binaryOp op = binaryOps [op]

binaryOps :: (Integral e , Stack e c) => [BinaryOperator] -> c -> c
binaryOps ops c = pushList (calculateOps e e' ops) c' where (e , e', c') = pop2 c

-- Stack instructions

halibut :: (Integral e , Stack e c) => c -> c
halibut c
  | i <= 0    = copy (negate i) c'
  | otherwise = move i c'
    where
      i = fromIntegral e
      (e , c') = pop1 c

move :: Stack e c => Index -> c -> c
move i c = c1 <> c2 <> c3 where
  (c1 , c3) = splitAt 1 c'
  (c2 , c') = splitAt i c

swap :: Stack e c => c -> c
swap c = push2 e' e c' where (e , e', c') = pop2 c

discard :: Stack e c => c -> c
discard = drop 1

slide :: Stack e c => Index -> c -> c
slide i c = push1 e (drop i c') where (e , c') = pop1 c

dup :: Stack e c => c -> c
dup = copy 0

copy :: Stack e c => Index -> c -> c
copy i c = push1 (c `index` i) c

-- Push instructions

pushChar1 :: (Num e , Stack e c) => Char -> c -> c
pushChar1 = genericPush1 . ord

genericPush1 :: (Integral v , Num e , Stack e c) => v -> c -> c
genericPush1 = push1 . fromIntegral

push1 ::  Stack e c => e -> c -> c
push1 e = pushList [e]

push2 :: Stack e c => e -> e -> c -> c
push2 e e' = pushList [e , e']

pushList :: Stack e c => [e] -> c -> c
pushList es c = fromList es <> c

----

index :: (Stack e c) => c -> Int -> e
index c i = check (c `indexMaybe` i) where
  check (Just e) = e
  check  Nothing = error $ "Empty stack " <> show c <> " index " <> show i

class (Semigroup c , Show c) => Stack e c | c -> e where
  fromList   :: [e] -> c
  empty      :: c
  indexMaybe :: c -> Index -> Maybe e
  lookup     :: Index -> c -> Maybe e
  splitAt    :: Index -> c -> (c , c)
  drop       :: Index -> c -> c
  pop1       :: c -> (e , c)
  pop2       :: c -> (e , e , c)
  empty      = fromList []
  indexMaybe = flip lookup
  lookup     = flip indexMaybe

instance Show e => Stack e [e] where
  fromList             = id
  empty                = []
  lookup        i   c  = c !!? i
  splitAt       i   c  = List.splitAt i c
  drop          i   c  = List.drop i c
  pop1         (e : c) = (e , c)
  pop1              c  = error $ "Empty c " <> show c
  pop2    (e : e' : c) = (e , e', c)
  pop2              c  = error $ "Empty c " <> show c

instance Show e => Stack e (Seq e) where
  fromList                 = Seq.fromList
  empty                    = Seq.empty
  lookup         i      c  = Seq.lookup i c
  splitAt        i      c  = Seq.splitAt i c
  drop           i      c  = Seq.drop i c
  pop1          (e :<|  c) = (e , c)
  pop1                   c = error $ "Empty c " <> show c
  pop2    (e :<| e' :<| c) = (e , e', c)
  pop2                  c  = error $ "Empty c " <> show c
