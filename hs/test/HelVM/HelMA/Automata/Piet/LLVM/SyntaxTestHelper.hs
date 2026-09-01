module HelVM.HelMA.Automata.Piet.LLVM.SyntaxTestHelper
  ( dl
  , dr
  , ll
  , lr
  , rl
  , rr
  , ul
  , ur
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Syntax

rl ∷ Course
rl = Course DPRight CCLeft

rr ∷ Course
rr = Course DPRight CCRight

dl ∷ Course
dl = Course DPDown CCLeft

dr ∷ Course
dr = Course DPDown CCRight

ll ∷ Course
ll = Course DPLeft CCLeft

lr ∷ Course
lr = Course DPLeft CCRight

ul ∷ Course
ul = Course DPUp CCLeft

ur ∷ Course
ur = Course DPUp CCRight
