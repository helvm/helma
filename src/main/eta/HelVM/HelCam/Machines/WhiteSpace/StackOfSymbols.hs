{-# Language AllowAmbiguousTypes   #-}
{-# Language FlexibleContexts      #-}
{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}
module HelVM.HelCam.Machines.WhiteSpace.StackOfSymbols where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorUtil
import HelVM.HelCam.Machines.WhiteSpace.Instruction

import HelVM.HelCam.Common.Memories.Stack

-- Arithmetic

binaryOp :: Stack Symbol m => BinaryOperator -> m -> m
binaryOp op stack = push1 (doBinary op symbol symbol' ::Symbol) stack' where (symbol, symbol', stack') = pop2 stack

-- Stack instructions

swap :: Stack Symbol m => m -> m
swap stack = push2 (symbol'::Symbol) symbol stack' where (symbol, symbol', stack') = pop2 stack

discard :: Stack Symbol m => m -> m
discard = drop' (0::Symbol) 1

slide :: Stack Symbol m => Index -> m -> m
slide i stack = push1 (symbol::Symbol) (drop' (0::Symbol) i stack') where (symbol, stack') = pop1 stack

dup :: Stack Symbol m => m -> m
dup = copy 0

copy :: Stack Symbol m => Index -> m -> m
copy i stack = push1 (select i stack ::Symbol) stack


