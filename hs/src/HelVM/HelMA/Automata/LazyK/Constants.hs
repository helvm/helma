module HelVM.HelMA.Automata.LazyK.Constants where

import           HelVM.HelMA.Automata.LazyK.Lambda

bCombinator :: Lambda
bCombinator = app3 S appKS K

appSelfApp :: Lambda -> Lambda
appSelfApp = app4 S I I

selfApp :: Lambda
selfApp = app3 S I I

app3SI :: Lambda -> Lambda
app3SI = app3 S I

appKS :: Lambda
appKS = App K S

appK :: Lambda -> Lambda
appK = App K

false :: Lambda
false = App K I

true :: Lambda
true = K
