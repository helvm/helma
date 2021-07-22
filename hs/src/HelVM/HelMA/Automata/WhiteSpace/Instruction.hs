module HelVM.HelMA.Automata.WhiteSpace.Instruction where

import           HelVM.HelMA.Automata.WhiteSpace.OperandParsers
import           HelVM.HelMA.Automata.WhiteSpace.Symbol

import           HelVM.HelMA.Automaton.BinaryOperator

import           Data.Vector                                    as Vector

type Index = Int

data Instruction =
    Liter !Integer
  | Copy  !Index
  | Slide !Index
  | Dup
  | Swap
  | Discard
  | Binary !BinaryOperator
  | Store --Save
  | Load  --Restore
  | Mark !Label
  | Call !Label
  | Jump !Label
  | Branch !BranchTest !Label
  | Return
  | OutputChar
  | OutputNum
  | InputChar
  | InputNum
  | End
  deriving stock (Eq , Show , Read)

type InstructionList   = [Instruction]
type InstructionVector = Vector Instruction

data BranchTest = EZ | Neg
   deriving stock (Eq , Show , Read)

----

parseIndex :: OperandParser Index
parseIndex = parseInt

parseSymbol :: OperandParser Symbol
parseSymbol = parseInteger

parseLabel :: Bool -> OperandParser Label
parseLabel False = parseDigitString
parseLabel True  = parseAsciiString
