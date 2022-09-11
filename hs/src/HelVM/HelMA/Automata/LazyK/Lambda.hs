module HelVM.HelMA.Automata.LazyK.Lambda where

import           Relude.Extra

app4 :: Lambda -> Lambda -> Lambda -> Lambda -> Lambda
app4 l1 l2 l3 l4 = l1 `App` l2 `App` l3 `App` l4

app3 :: Lambda -> Lambda -> Lambda -> Lambda
app3 l1 l2 l3 = l1 `App` l2 `App` l3

foldlLambda :: NonEmpty Lambda -> Lambda
foldlLambda = foldl1' App

data Lambda =
    S
  | K
  | I
  | App Lambda Lambda
  | Succ
  | Number !Natural
  | Var Text
  deriving stock (Eq , Read , Show)
