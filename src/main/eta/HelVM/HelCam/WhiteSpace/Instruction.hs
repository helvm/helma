module HelVM.HelCam.WhiteSpace.Instruction where

import HelVM.HelCam.WhiteSpace.OperandParsers

data Instruction =
    Const Value
  | Dup
  | Ref Index
  | Slide Index
  | Swap
  | Discard
  | Binary BinaryOperator
  | Store
  | Load
  | Label Identifier
  | Call Identifier
  | Jump Identifier
  | Branch BranchTest Identifier
  | Return
  | OutputChar
  | OutputNum
  | InputChar
  | InputNum
  | End
  deriving (Eq, Show, Read)

type InstructionList = [Instruction]

data BinaryOperator = Add | Sub | Mul | Div | Mod
   deriving (Eq, Show, Read)

data BranchTest = EZ | Neg
   deriving (Eq, Show, Read)

----

type Address = Int

type Index = Int
parseIndex :: OperandParser Index
parseIndex = parseInt

type Value = Integer
parseValue :: OperandParser Value
parseValue = parseInteger

type Identifier = String
parseIdentifier :: Bool -> OperandParser Identifier
parseIdentifier False = parseBitString
parseIdentifier True  = parseAsciiString
