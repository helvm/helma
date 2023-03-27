module HelVM.HelMA.Automaton.PrettyPrinter where

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Instruction.CFInstruction

printIL :: InstructionList -> Text
printIL il = unlines $ printI <$> il

printI :: Instruction -> Text
printI (IAL i) = show i
printI (ILS i) = show i
printI (ICF i) = printCF i
printI End     = show End

printCF :: CFInstruction -> Text
printCF i@(Mark _) = "\n" <> show i
printCF i          = show i
