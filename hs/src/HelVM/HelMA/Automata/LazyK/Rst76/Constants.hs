module HelVM.HelMA.Automata.LazyK.Rst76.Constants where

import           HelVM.HelMA.Automata.LazyK.Constants
import           HelVM.HelMA.Automata.LazyK.Lambda

cons :: Lambda
cons = delete (Var "x") $ consX $ Var "x"

consX :: Lambda -> Lambda
consX x = delete (Var "y") $ consXY x $ Var "y"

consXY :: Lambda -> Lambda -> Lambda
consXY x y = S `App` (si `App` (K `App` x)) `App` (K `App` y)

delete :: Lambda -> Lambda -> Lambda
delete v (App x y) | elem' v x && elem' v y = S `App` delete v x `App` delete v y
delete v (App x y) | elem' v x = S `App` delete v x `App` App K y
delete v (App x y) | v == y = x
delete v (App x y) | elem' v y = S `App` App K x `App` delete v y
delete v x | v == x = I
delete v x = error $ show v <> "" <> show x

elem' :: Lambda -> Lambda -> Bool
elem' v x | v == x = True
elem' v (App x y) = elem' v x ||  elem' v y
elem' _ _ = False

nil :: Lambda
nil = K `App` true

isNil :: Lambda
isNil = K `App` (K `App` false)

carX :: Lambda -> Lambda
carX x = x `App` true

cdrX :: Lambda -> Lambda
cdrX x = x `App` false

car :: Lambda
car = si `App` (K `App` true)

cdr :: Lambda
cdr = si `App` (K `App` false)

nthNX :: Lambda -> Lambda -> Lambda
nthNX n x = carX (n `App` cdrX x)

b :: Lambda
b = S `App` (K `App` S) `App` K

m :: Lambda
m = S `App` I `App` I

t :: Lambda
t = S `App` (K `App` si) `App` K

----
ssssss :: Lambda
ssssss = ssss `App` S `App` S

ssiss :: Lambda
ssiss = S `App` (S `App` I `App` S) `App` S

ssss :: Lambda
ssss = ss `App` S `App` S

ssss1 :: Lambda
ssss1 = App S  sss

ssf :: Lambda
ssf = App ss  false

ssi :: Lambda
ssi = App ss  I

sss :: Lambda
sss = App ss S

si :: Lambda
si = App S I

ss :: Lambda
ss = App S S
