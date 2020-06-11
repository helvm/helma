module HelVM.HelMA.Automata.ETA.StackOfSymbols where

import HelVM.HelMA.Automata.ETA.EvaluatorUtil

import HelVM.HelMA.Common.Memories.Stack

-- Arithmetic

divMod :: Stack Symbol m => m -> m
divMod stack = push2 (symbol' `mod` symbol ::Symbol) (symbol' `div` symbol ::Symbol) stack'
  where (symbol, symbol', stack') = pop2 stack

sub :: Stack Symbol m => m -> m
sub stack = push1 (symbol' - symbol ::Symbol) stack'
    where (symbol, symbol', stack') = pop2 stack

-- Stack instructions

halibut :: Stack Symbol m => m -> m
halibut stack
  | i <= 0     = copy (negate i) stack'
  | otherwise  = move (0 ::Symbol) i stack'
    where (i, stack') = pop1 stack

move :: Stack Symbol m => Symbol -> Index -> m -> m
move symbol i stack = tops <> middles <> bottoms where
  (middles, stack')  = splitAt' symbol i stack
  (tops, bottoms)    = splitAt' symbol 1 stack'

copy :: Stack Symbol m => Index -> m -> m
copy i stack = push1 (select i stack ::Symbol) stack
