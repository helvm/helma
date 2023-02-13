module HelVM.HelMA.Automaton.Instruction.SInstruction where

import           HelVM.HelMA.Automaton.Instruction.IOInstruction

-- | Constructors

blAnd :: OperatorType -> BinaryInstruction
blAnd Bitwise = BAnd
blAnd Logical = LAnd

blOr :: OperatorType -> BinaryInstruction
blOr Bitwise = BOr
blOr Logical = LOr

blXor :: OperatorType -> BinaryInstruction
blXor Bitwise = BXor
blXor Logical = LXor

blEQ :: OperatorType -> BinaryInstruction
blEQ Bitwise = BEQ
blEQ Logical = LEQ

blGT :: OperatorType -> BinaryInstruction
blGT Bitwise = BGT
blGT Logical = LGT

-- | Other functions

calculateOps :: Integral a => a -> a -> [BinaryInstruction] -> [a]
calculateOps operand operand' = map (calculateOp operand operand')

calculateOp :: Integral a => a -> a -> BinaryInstruction -> a
calculateOp operand operand' operation = doBinary operation operand' operand

doBinary :: Integral a => BinaryInstruction -> a -> a -> a
doBinary Add = (+)
doBinary Sub = (-)
doBinary Mul = (*)
doBinary Div = div
doBinary Mod = mod
doBinary o   = error $ show o

-- | Types
data SInstruction =
    SAL      !ALInstruction
  | SIO      !IOInstruction
  deriving stock (Eq , Read , Show)

data ALInstruction =
    Cons      Integer
  | Unary    !UnaryInstruction
  | Binary   !BinaryInstruction
  | Binaries [BinaryInstruction]
  | SStatic  !StackIndex !ManipulationInstruction
  | SDynamic             !ManipulationInstruction
  | Halibut
  | Pick
  | Discard
  deriving stock (Eq , Read , Show)

data UnaryInstruction = Neg | BNot | LNot
  deriving stock (Eq , Read , Show)

data BinaryInstruction =
     Add | Sub | Mul | Div | Mod
  | BAnd | BOr | BXor | BEQ | BGT
  | LAnd | LOr | LXor | LEQ | LGT
  deriving stock (Eq , Read , Show)

data ManipulationInstruction = Copy | Move | Slide
  deriving stock (Eq , Read , Show)

type StackIndex = Int

data OperatorType = Bitwise | Logical
