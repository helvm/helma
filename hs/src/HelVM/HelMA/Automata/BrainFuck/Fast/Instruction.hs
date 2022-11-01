module HelVM.HelMA.Automata.BrainFuck.Fast.Instruction where

import qualified Data.DList  as D
import qualified Data.Vector as V

data SomeInstruction =
    Move Int
  | Inc Int
  | Output
  | Input
  | While FastInstructionVector
  deriving stock (Eq , Read , Show)

type SomeInstructionList   = [SomeInstruction]
type SomeInstructionDList  = D.DList SomeInstruction
type FastInstructionVector = V.Vector SomeInstruction
