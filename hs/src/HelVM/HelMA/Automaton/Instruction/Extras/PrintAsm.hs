module HelVM.HelMA.Automaton.Instruction.Extras.PrintAsm where

class PrintAsm a where
  printAsm :: a -> Text
