module HelVM.HelMA.Automata.WhiteSpace.Addressing where

import HelVM.HelMA.Automata.WhiteSpace.Instruction
import HelVM.HelMA.Automata.WhiteSpace.Symbol

type InstructionCounter = InstructionAddress
newtype InstructionStack = IS [InstructionAddress]
  deriving stock (Show)

data InstructionUnit = IU InstructionList InstructionCounter InstructionStack
  deriving stock (Show)

isNotJump :: Integral e => BranchTest -> e -> Bool
isNotJump t e = not $ isJump t e

isJump :: Integral e => BranchTest -> e -> Bool
isJump EZ  e = e == 0
isJump Neg e = e < 0

findAddress :: InstructionList -> Label -> InstructionAddress
findAddress = findAddress' 0

findAddress' :: InstructionAddress -> InstructionList -> Label -> InstructionAddress
findAddress' _       []             l = error $ "Undefined label (" <> show l  <> ")" --FIXME
findAddress' address ((Mark l'):il) l
  | l == l'                           = address
  | otherwise                         = findAddress' (address+1) il l
findAddress' address (_:il)         l = findAddress' (address+1) il l
