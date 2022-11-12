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
  | MulAddClr Integer Integer
  | DupClr Integer Integer
  | MulDupClr Integer Integer Integer Integer
  | TriClr Integer Integer Integer
  deriving stock (Eq , Read , Show)

type FastInstructionList = [FastInstruction]
