module HelVM.HelMA.Automaton.Operator.BinaryOperator where

import           HelVM.HelMA.Automaton.BIntegral
import           HelVM.HelMA.Automaton.Operator.Util

import           Data.Bits                           hiding (xor)

-- | Constructors

blAnd :: Bool -> BinaryOperator
blAnd False = BAnd
blAnd True  = LAnd

blOr :: Bool -> BinaryOperator
blOr False = BOr
blOr True  = LOr

blXor :: Bool -> BinaryOperator
blXor False = BXor
blXor True  = LXor

blEQ :: Bool -> BinaryOperator
blEQ False = BEQ
blEQ True  = LEQ

blGT :: Bool -> BinaryOperator
blGT False = BGT
blGT True  = LGT



-- | Others

calculateOps :: BIntegral a => a -> a -> [BinaryOperator] -> [a]
calculateOps operand operand' = map (calculateOp operand operand')

calculateOp :: BIntegral a => a -> a -> BinaryOperator -> a
calculateOp operand operand' operation = doBinary operation operand' operand

--dynamicDoBinary :: DynamicIntegral a => BinaryOperator -> [a] -> [a]
--dynamicDoBinary op stack --TODO

doBinary :: BIntegral a => BinaryOperator -> a -> a -> a
doBinary Add  = (+)
doBinary Sub  = (-)
doBinary Mul  = (*)
doBinary Div  = div
doBinary Mod  = mod

doBinary BAnd = (.&.)
doBinary BOr  = (.|.)
doBinary BXor = xor
doBinary BEQ  = bEq
doBinary BGT  = bGt

doBinary LAnd = lAnd
doBinary LOr  = lOr
doBinary LXor = lXor
doBinary LEQ  = lEq
doBinary LGT  = lGt

bEq :: Integral a => a -> a -> a
bEq o1 o2 = boolToBits $ o1 == o2

bGt :: Integral a => a -> a -> a
bGt o1 o2 = boolToBits $ o1 > o2

lAnd :: Integral a => a -> a -> a
lAnd 0 0 = 0
lAnd _ 0 = 0
lAnd 0 _ = 0
lAnd _ _ = 1

lOr :: Integral a => a -> a -> a
lOr 0 0 = 0
lOr _ 0 = 1
lOr 0 _ = 1
lOr _ _ = 1

lXor :: Integral a => a -> a -> a
lXor 0 0 = 0
lXor _ 0 = 1
lXor 0 _ = 1
lXor _ _ = 0

lEq :: Integral a => a -> a -> a
lEq o1 o2 = boolToIntegral $ o1 == o2

lGt :: Integral a => a -> a -> a
lGt o1 o2 = boolToIntegral $ o1 > o2

-- | Types

data BinaryOperator =
    Add | Sub | Mul | Div | Mod
  | BAnd | BOr | BXor | BEQ | BGT
  | LAnd | LOr | LXor | LEQ | LGT
  deriving stock (Eq , Show , Read)
