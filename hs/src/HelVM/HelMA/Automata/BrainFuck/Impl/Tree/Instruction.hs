module HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Instruction where

import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction

import           Data.DList
import           Data.Vector

data TreeInstruction =
    Simple SimpleInstruction
  | While !TreeInstructionVector
  deriving stock (Eq , Read , Show)

type TreeInstructionList   = [TreeInstruction]
type TreeInstructionDList  = DList TreeInstruction
type TreeInstructionVector = Vector TreeInstruction
