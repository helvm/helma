module HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Instruction where

data FastInstruction =
    Move Integer
  | Inc Integer
  | Output
  | Input
  | While FastInstructionList
  | Set Integer
  | SubClr Integer
  | AddClr Integer
  | DupClr Integer Integer
  | TriClr Integer Integer Integer
  deriving stock (Eq , Read , Show)

type FastInstructionList = [FastInstruction]
