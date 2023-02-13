module HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.PrintAsm
-- | Types

-- TODO convert to (Output/Input) (Char/Dec)

data IOInstruction =
    OutputChar
  | OutputDec
  | InputChar
  | InputDec
  deriving stock (Eq , Read , Show)

-- | Type Classes

instance PrintAsm IOInstruction where
  printAsm = printIO

-- | Internal

printIO :: IOInstruction -> Text
printIO OutputChar = "outputC"
printIO OutputDec  = "outputD"
printIO InputChar  = "inputC"
printIO InputDec   = "inputD"
