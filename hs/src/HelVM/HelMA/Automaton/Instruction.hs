module HelVM.HelMA.Automaton.Instruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.TextExtra

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           Data.Vector                                            as Vector

-- | Types

data Instruction =
    IAL !SMInstruction
  | ILS !LSInstruction
  | ICF !CFInstruction
  | End
  deriving stock (Eq , Read , Show)

type InstructionList   = [Instruction]
type InstructionVector = Vector Instruction

-- | Internal

printIL :: InstructionList -> Text
printIL il = unlines $ printI <$> il

printI :: Instruction -> Text
printI (IAL i) = printSM i
printI (ICF i) = printCF i
printI (ILS i) = toLowerShow i
printI  End    = toLowerShow End
