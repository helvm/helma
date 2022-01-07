module HelVM.HelMA.Automaton.Operator.UnaryOperator where

import           HelVM.HelMA.Automaton.BIntegral
import           HelVM.HelMA.Automaton.Dynamic.DynamicIntegral

import           Data.Bits

blNot:: Bool -> UnaryOperator
blNot False = BNot
blNot True  = LNot

dynamicDoUnary2 :: DynamicIntegral i a => UnaryOperator -> [a] -> [a]
dynamicDoUnary2 op (a : s) = dynamicDoUnary op a : s
dynamicDoUnary2 op     []  = error $ "dynamicDoUnary2 op" <> show op

dynamicDoUnary :: DynamicIntegral i a => UnaryOperator -> a -> a
dynamicDoUnary op = aaa (doUnary op)

doUnary :: BIntegral a => UnaryOperator -> a -> a
doUnary Neg  = negate
doUnary BNot = complement
doUnary LNot = lNot

lNot :: Integral a => a -> a
lNot 0 = 1
lNot _ = 0

data UnaryOperator = Neg | BNot | LNot
  deriving stock (Eq , Show , Read)
