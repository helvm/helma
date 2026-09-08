module HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common

-- | Types

data IOInstruction
  = OutputChar
  | OutputDec
  | OutputCharMaybe
  | OutputDecMaybe
  | InputChar
  | InputDec
  deriving stock (Eq, Read, Show)

-- | Type Classes

instance PrintAsm IOInstruction where
  printAsm = printIO

-- | Internal

printIO ∷ IOInstruction → Text
printIO OutputChar      = "outputC"
printIO OutputDec       = "outputD"
printIO OutputCharMaybe = "outputCMaybe"
printIO OutputDecMaybe  = "outputDMaybe"
printIO InputChar       = "inputC"
printIO InputDec        = "inputD"
