module HelVM.HelMA.Automata.WhiteSpace.EvaluatorUtil where

import HelVM.HelMA.Automata.WhiteSpace.Instruction

type InstructionCounter = InstructionAddress
newtype InstructionStack = IS [InstructionAddress]
  deriving (Show)

data InstructionUnit = IU InstructionList InstructionCounter InstructionStack
  deriving (Show)

doBranchTest :: Integral s => BranchTest -> s -> Bool
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
