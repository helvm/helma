module HelVM.HelMA.Automaton.Instruction.SInstruction where

import           HelVM.HelMA.Automaton.Instruction.IOInstruction

-- | Constructors

blAnd :: OperatorType -> BinaryOperation
blAnd Bitwise = BAnd
blAnd Logical = LAnd

blOr :: OperatorType -> BinaryOperation
blOr Bitwise = BOr
blOr Logical = LOr

blXor :: OperatorType -> BinaryOperation
blXor Bitwise = BXor
blXor Logical = LXor

blEQ :: OperatorType -> BinaryOperation
blEQ Bitwise = BEQ
blEQ Logical = LEQ

blGT :: OperatorType -> BinaryOperation
blGT Bitwise = BGT
blGT Logical = LGT

-- | Other functions

calculateOps :: Integral a => a -> a -> [BinaryOperation] -> [a]
calculateOps operand operand' = map (calculateOp operand operand')

calculateOp :: Integral a => a -> a -> BinaryOperation -> a
calculateOp operand operand' operation = doBinary operation operand' operand

doBinary :: Integral a => BinaryOperation -> a -> a -> a
doBinary Add = (+)
doBinary Sub = (-)
doBinary Mul = (*)
doBinary Div = div
doBinary Mod = mod
doBinary o   = error $ show o

-- | Types
data SInstruction =
    SPure    !SPureInstruction
  | SIO      !IOInstruction
  deriving stock (Eq , Read , Show)

data SPureInstruction =
    Cons     !Integer
  | Unary    !UnaryOperation
  | Binary   !BinaryOperation
  | Binaries [BinaryOperation]
  | Indexed  !IndexedOperation !IndexOperand
  | Halibut
  | Pick
  | Discard
  deriving stock (Eq , Read , Show)

data IndexOperand = TopO | ImmediateO !Index
  deriving stock (Eq , Read , Show)

data UnaryOperation = Neg | BNot | LNot
  deriving stock (Eq , Read , Show)

data BinaryOperation =
     Add | Sub | Mul | Div | Mod
  | BAnd | BOr | BXor | BEQ | BGT
  | LAnd | LOr | LXor | LEQ | LGT
  deriving stock (Eq , Read , Show)

data IndexedOperation = Copy | Move | Slide
  deriving stock (Eq , Read , Show)

type Index = Int

data OperatorType = Bitwise | Logical
