module HelVM.HelMA.Automata.LazyK.Reducer (
  reduce,
  flippedApply,
  apply,
) where

import           HelVM.HelMA.Automata.LazyK.Lambda

reduce :: Lambda -> Lambda
reduce (App x y) = reduce x `apply` reduce y
reduce  x        = x

flippedApply :: Lambda -> Lambda -> Lambda
flippedApply = flip apply

apply :: Lambda -> Lambda -> Lambda
apply (S `App` x `App` y) z = apply x z `apply` apply y z
apply (App K x) _           = x
apply I x                   = x
apply Succ (Number x)       = Number $! x + 1
apply Succ x                = error $ "attempted to apply inc to a non-number " <> show x
apply f x                   = App f x
