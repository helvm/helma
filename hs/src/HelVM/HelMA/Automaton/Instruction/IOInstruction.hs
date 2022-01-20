module HelVM.HelMA.Automaton.Instruction.IOInstruction where

data IOInstruction =
    OutputChar
  | OutputDec
  | InputChar
  | InputDec
  deriving stock (Eq , Show , Read)
