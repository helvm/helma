module HelVM.HelMA.Automata.WhiteSpace.Addressing where

import HelVM.HelMA.Automata.WhiteSpace.Instruction
import HelVM.HelMA.Automata.WhiteSpace.Symbol

import HelVM.Common.Safe

type InstructionCounter = InstructionAddress
newtype InstructionStack = IS [InstructionAddress]
  deriving stock (Show)

data InstructionUnit = IU !InstructionList !InstructionCounter !InstructionStack
  deriving stock (Show)

isNotJump :: Integral e => BranchTest -> e -> Bool
isNotJump t e = not $ isJump t e

isJump :: Integral e => BranchTest -> e -> Bool
isJump EZ  e = e == 0
isJump Neg e = e < 0

findAddress :: MonadSafeError m => InstructionList -> Label -> m InstructionAddress
findAddress = findAddress' 0

findAddress' :: MonadSafeError m => InstructionAddress -> InstructionList -> Label -> m InstructionAddress
findAddress' _       []             l = liftError $ "Undefined label (" <> show l  <> ")"
findAddress' address ((Mark l'):il) l
  | l == l'                           = pure address
  | otherwise                         = findAddress' (address+1) il l
findAddress' address (_:il)         l = findAddress' (address+1) il l
