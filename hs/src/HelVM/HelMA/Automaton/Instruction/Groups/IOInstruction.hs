module HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction where

import           Prettyprinter ( Pretty (pretty) )

-- | Types

data IOInstruction
  = OutputChar
  | OutputDec
  | OutputCharMaybe
  | OutputDecMaybe
  | InputChar
  | InputDec
  deriving stock (Eq, Read, Show)

-- | Pretty instance

instance Pretty IOInstruction where
  pretty OutputChar      = "outputC"
  pretty OutputDec       = "outputD"
  pretty OutputCharMaybe = "outputCMaybe"
  pretty OutputDecMaybe  = "outputDMaybe"
  pretty InputChar       = "inputC"
  pretty InputDec        = "inputD"

-- | Internal (opcjonalny wrapper, jeśli nadal używasz printIO w innych miejscach)

printIO ∷ IOInstruction → Text
printIO OutputChar      = "outputC"
printIO OutputDec       = "outputD"
printIO OutputCharMaybe = "outputCMaybe"
printIO OutputDecMaybe  = "outputDMaybe"
printIO InputChar       = "inputC"
printIO InputDec        = "inputD"
