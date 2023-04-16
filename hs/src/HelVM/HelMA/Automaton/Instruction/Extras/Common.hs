module HelVM.HelMA.Automaton.Instruction.Extras.Common where

class PrintAsm a where
  printAsm :: a -> Text

type Index = Int
