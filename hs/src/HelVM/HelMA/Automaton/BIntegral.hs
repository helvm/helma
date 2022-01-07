module HelVM.HelMA.Automaton.BIntegral where

import           Data.Bits

type BIntegral a = (Show a , Read a , Bits a , Integral a)
