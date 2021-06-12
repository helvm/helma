module HelVM.HelMA.Automaton.BinaryOperator where

calculateOps :: Integral a => a -> a -> [BinaryOperator] -> [a]
calculateOps operand operand' = map (calculateOp operand operand')

calculateOp :: Integral a => a -> a -> BinaryOperator -> a
calculateOp operand operand' operation = doBinary operation operand' operand

doBinary :: Integral a => BinaryOperator -> a -> a -> a
doBinary Add = (+)
doBinary Sub = (-)
doBinary Mul = (*)
doBinary Div = div
doBinary Mod = mod

data BinaryOperator = Add | Sub | Mul | Div | Mod
  deriving stock (Eq , Show , Read)
