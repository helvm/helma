module HelVM.HelMA.Automata.BrainFuck.Instruction.SimpleInstruction where

import qualified Text.Show

data SimpleInstruction =
    MoveR
  | MoveL
  | Inc
  | Dec
  | Output
  | Input
  deriving stock (Eq , Ord, Enum)

instance Show SimpleInstruction where
  show MoveR  = ">"
  show MoveL  = "<"
  show Inc    = "+"
  show Dec    = "-"
  show Output = "."
  show Input  = ","
