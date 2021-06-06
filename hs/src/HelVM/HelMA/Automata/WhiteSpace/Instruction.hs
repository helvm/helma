module HelVM.HelMA.Automata.WhiteSpace.Instruction where

import HelVM.HelMA.Automata.WhiteSpace.OperandParsers

import HelVM.HelMA.Common.BinaryOperator
import HelVM.HelMA.Common.Memories.StackConst

data Instruction =
    Liter Integer
  | Copy  Index
  | Slide Index
  | Dup
  | Swap
  | Discard
  | Binary BinaryOperator
  | Store --Save
  | Load  --Restore
  | Mark Label
  | Call Label
  | Jump Label
  | Branch BranchTest Label
  | Return
  | OutputChar
  | OutputNum
  | InputChar
  | InputNum
  | End
  deriving (Eq , Show , Read)

type InstructionList = [Instruction]

data BranchTest = EZ | Neg
   deriving (Eq , Show , Read)

----

type InstructionAddress = Int

parseIndex :: OperandParser Index
parseIndex = parseInt

type Symbol = Integer
parseSymbol :: OperandParser Symbol
parseSymbol = parseInteger

type SymbolList = [Symbol]

type Label = String
parseLabel :: Bool -> OperandParser Label
parseLabel False = parseBitString
parseLabel True  = parseAsciiString
