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

import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax

rl ∷ DPCC
rl = DPCC DPRight CCLeft

rr ∷ DPCC
rr = DPCC DPRight CCRight

dl ∷ DPCC
dl = DPCC DPDown CCLeft

dr ∷ DPCC
dr = DPCC DPDown CCRight

ll ∷ DPCC
ll = DPCC DPLeft CCLeft

lr ∷ DPCC
lr = DPCC DPLeft CCRight

ul ∷ DPCC
ul = DPCC DPUp CCLeft

ur ∷ DPCC
ur = DPCC DPUp CCRight
