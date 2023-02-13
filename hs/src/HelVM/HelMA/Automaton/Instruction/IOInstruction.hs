module HelVM.HelMA.Automaton.Instruction.IOInstruction where

-- | Types

data IOInstruction =
    OutputChar
  | OutputDec
  | InputChar
  | InputDec
  deriving stock (Eq , Read , Show)
