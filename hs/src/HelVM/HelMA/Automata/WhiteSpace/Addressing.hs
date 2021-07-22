module HelVM.HelMA.Automata.WhiteSpace.Addressing where

import           HelVM.HelMA.Automata.WhiteSpace.Instruction
import           HelVM.HelMA.Automata.WhiteSpace.Symbol

import           HelVM.Common.Safe

import qualified Data.Vector                                 as Vector

type InstructionCounter = InstructionAddress
newtype InstructionStack = IS [InstructionAddress]
  deriving stock (Show)

data InstructionUnit = IU !InstructionVector !InstructionCounter !InstructionStack
  deriving stock (Show)

isNotJump :: Integral e => BranchTest -> e -> Bool
isNotJump t e = not $ isJump t e

isJump :: Integral e => BranchTest -> e -> Bool
isJump EZ  e = e == 0
isJump Neg e = e < 0

findAddress :: MonadSafeError m => InstructionVector -> Label -> m InstructionAddress
findAddress il l = liftMaybeOrError ("Undefined label (" <> show l  <> ")") $ Vector.findIndex (Mark l ==) il