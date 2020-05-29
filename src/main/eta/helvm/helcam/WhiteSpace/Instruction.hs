module HelVM.HelCam.WhiteSpace.Instruction where

import Numeric.Natural

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

type Value      = Integer
type Index      = Int
type Address    = Int
type Identifier = Natural
