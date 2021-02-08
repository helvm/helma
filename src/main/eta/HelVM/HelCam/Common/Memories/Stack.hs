{-# Language AllowAmbiguousTypes   #-}
{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}
module HelVM.HelCam.Common.Memories.Stack (
  Index,
  Stack,
  select,
  HelVM.HelCam.Common.Memories.Stack.empty,
  HelVM.HelCam.Common.Memories.Stack.lookup,
  HelVM.HelCam.Common.Memories.Stack.splitAt',
  HelVM.HelCam.Common.Memories.Stack.drop',
  push1,
  pop1,
  push2,
  pop2
) where

import Data.Sequence as Seq

type Index = Int

select :: Stack s m => Index -> m -> s
select i stack = check $ HelVM.HelCam.Common.Memories.Stack.lookup i stack where
  check (Just symbol) = symbol
  check  Nothing      = error $ "Empty stack " <> show stack <> " index " <> show i

class (Semigroup m, Show m) => Stack s m where
  empty    :: m
  lookup   :: Index -> m -> Maybe s
  splitAt' :: s -> Index -> m -> (m, m)
  drop'    :: s -> Index -> m -> m
  push1    :: s -> m -> m
  pop1     :: m -> (s, m)
  push2    :: s -> s -> m -> m
  pop2     :: m -> (s, s, m)

instance Show s => Stack s [s] where
  empty                              = []
  lookup            i         stack  = stack !!? i
  splitAt' _        i         stack  = Prelude.splitAt i stack
  drop'    _        i         stack  = Prelude.drop i stack
  push1             symbol    stack  = symbol: stack
  pop1             (symbol  : stack) = (symbol, stack)
  pop1                        stack  = error $ "Empty stack " <> show stack
  push2    symbol   symbol'   stack  = symbol: symbol': stack
  pop2    (symbol : symbol' : stack) = (symbol, symbol', stack)
  pop2                        stack  = error $ "Empty stack " <> show stack

instance Show s => Stack s (Seq s) where
  empty                                  = Seq.fromList []
  lookup              i           stack  = Seq.lookup i stack
  splitAt' _          i           stack  = Seq.splitAt i stack
  drop'    _          i           stack  = Seq.drop i stack
  push1               symbol      stack  = symbol <| stack
  pop1               (symbol :<|  stack) = (symbol, stack)
  pop1                            stack  = error $ "Empty stack " <> show stack
  push2    symbol     symbol'     stack  = symbol <| symbol' <| stack
  pop2    (symbol :<| symbol' :<| stack) = (symbol, symbol', stack)
  pop2                            stack  = error $ "Empty stack " <> show stack
