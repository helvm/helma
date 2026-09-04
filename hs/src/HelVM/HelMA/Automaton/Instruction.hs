module HelVM.HelMA.Automaton.Instruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.TextExtra

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Control.Safe

import           Data.List.Index
import qualified Data.Vector                                            as Vector

import qualified Data.Text.Lazy.Builder                                 as LText

-- | Types

data Instruction
  = ISM !SMInstruction
  | ILS !LSInstruction
  | ICF !CFInstruction
  | End
  deriving stock (Eq, Read, Show)

type InstructionList   = [Instruction]
type InstructionVector = Vector.Vector Instruction

-- | print

renderILSafe ∷ Safe InstructionList → LText
renderILSafe = either (toLText . errorsToText) renderIL

renderIL ∷ InstructionList → LText
renderIL = LText.toLazyText . printILBuilder

printILBuilder ∷ InstructionList → LText.Builder
printILBuilder = foldMap printlnIBuilder

printlnIBuilder ∷ Instruction → LText.Builder
printlnIBuilder  i= printIBuilder i <> "\n"

printIBuilder ∷ Instruction → LText.Builder
printIBuilder (ISM i) = LText.fromText (printSM i)
printIBuilder (ICF i) = LText.fromText (printCF i)
printIBuilder (ILS i) = LText.fromText (toLowerShow i)
printIBuilder  End    = "end"


printIndexedIL ∷ InstructionList → Text
printIndexedIL il = unlines $ printIndexedI <$> indexed il

printIndexedI ∷ (Int , Instruction) → Text
printIndexedI (index , i) = printI i <> " # " <> show index

printIL ∷ InstructionList → Text
printIL il = unlines $ printI <$> il

printI ∷ Instruction → Text
printI (ISM i) = printSM i
printI (ICF i) = printCF i
printI (ILS i) = toLowerShow i
printI  End    = toLowerShow End
