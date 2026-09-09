module HelVM.HelMA.Automaton.Instruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.TextExtra

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           Data.List.Index
import qualified Data.Vector                                            as Vector

import           Prettyprinter                                          ( Pretty (pretty), (<+>) )
import qualified Prettyprinter                                          as PP
import qualified Prettyprinter.Render.Text                              as PP.Text

-- | Types

data Instruction
  = ISM !SMInstruction
  | ILS !LSInstruction
  | ICF !CFInstruction
  | End
  deriving stock (Eq, Read, Show)

type InstructionList   = [Instruction]
type InstructionVector = Vector.Vector Instruction

-- | Pretty instance

instance Pretty Instruction where
  pretty (ISM i) = pretty $ printSM i
  pretty (ICF i) = pretty $ printCF i
  pretty (ILS i) = pretty $ toLowerShow i
  pretty  End    = pretty $ toLowerShow End

-- | print

printIndexedIL ∷ InstructionList → LText
printIndexedIL il = PP.Text.renderLazy $ PP.layoutCompact $ PP.vsep (printIndexedI <$> indexed il) <> PP.line

printIndexedI ∷ (Int, Instruction) → PP.Doc ann
printIndexedI (index, i) = pretty i <+> "#" <+> pretty index

printIL ∷ InstructionList → LText
printIL il = PP.Text.renderLazy $ PP.layoutCompact $ PP.vsep (pretty <$> il) <> PP.line

printI ∷ Instruction → LText
printI i = PP.Text.renderLazy $ PP.layoutCompact $ pretty i <> PP.line
