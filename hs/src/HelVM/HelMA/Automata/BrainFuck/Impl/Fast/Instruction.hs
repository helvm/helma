module HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Instruction where

data FastInstruction =
    Move Int
  | Inc Int
  | Output
  | Input
  | While FastInstructionList
  | Clear
  | Set Int
  deriving stock (Eq , Read , Show)

type FastInstructionList   = [FastInstruction]
