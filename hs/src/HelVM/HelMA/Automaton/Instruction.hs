module HelVM.HelMA.Automaton.Instruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.TextExtra

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           Data.List.Index
import qualified Data.Vector                                            as Vector


-- | Types

data Instruction =
    ISM !SMInstruction
  | ILS !LSInstruction
  | ICF !CFInstruction
  | End
  deriving stock (Eq , Read , Show)

type InstructionList   = [Instruction]
type InstructionVector = Vector.Vector Instruction

-- | print

printIndexedIL :: InstructionList -> Text
printIndexedIL il = unlines $ printIndexedI <$> indexed il

printIndexedI :: (Int , Instruction) -> Text
printIndexedI (index , i) = printI i <> " # " <> show index

printIL :: InstructionList -> Text
printIL il = unlines $ printI <$> il

printI :: Instruction -> Text
printI (ISM i) = printSM i
printI (ICF i) = printCF i
printI (ILS i) = toLowerShow i
printI  End    = toLowerShow End
