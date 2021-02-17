module HelVM.HelCam.Machines.WhiteSpace.EvaluatorUtil where

import HelVM.HelCam.Machines.WhiteSpace.Instruction

newtype Stack = Stack [Symbol]

type InstructionCounter = InstructionAddress
newtype InstructionStack = IS [InstructionAddress]
  deriving (Show)

data InstructionUnit = IU InstructionList InstructionCounter InstructionStack
  deriving (Show)

doBinary :: BinaryOperator -> Symbol -> Symbol -> Symbol
doBinary Add s s' = s' + s
doBinary Sub s s' = s' - s
doBinary Mul s s' = s' * s
doBinary Div s s' = s' `div` s
doBinary Mod s s' = s' `mod` s

doBranchTest :: BranchTest -> Symbol -> Bool
doBranchTest EZ  s = s == 0
doBranchTest Neg s = s < 0

findAddress :: InstructionList -> Label -> InstructionAddress
findAddress = findAddress' 0

findAddress' :: InstructionAddress -> InstructionList -> Label -> InstructionAddress
findAddress' _       []             l = error $ "Undefined label (" <> show l  <> ")"
findAddress' address ((Mark l'):il) l
  | l == l'                           = address
  | otherwise                         = findAddress' (address+1) il l
findAddress' address (_:il)         l = findAddress' (address+1) il l
