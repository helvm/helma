module HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Extras.TextExtra
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction

import           HelVM.HelIO.Containers.Extra

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
data SMInstruction =
    SPure    !SPureInstruction
  | SIO      !IOInstruction
  deriving stock (Eq , Read , Show)

data SPureInstruction =
    Cons     !Integer
  | Unary    !UnaryOperation
  | Binary   !BinaryOperation
  | Binaries [BinaryOperation]
  | Indexed  !IndexOperand !IndexedOperation
  | Halibut
  | Pick
  | Discard
  deriving stock (Eq , Read , Show)

data IndexOperand = ITop | IImmediate !Index
  deriving stock (Eq , Read , Show)

data UnaryOperation = Neg | BNot | LNot | UImmediate Integer BinaryOperation
  deriving stock (Eq , Read , Show)

data BinaryOperation =
     Add | Sub | Mul | Div | Mod
  | BAnd | BOr | BXor | BEQ | BGT
  | LAnd | LOr | LXor | LEQ | LGT
  deriving stock (Eq , Read , Show)

data IndexedOperation = Copy | Move | Slide
  deriving stock (Eq , Read , Show)

data OperatorType = Bitwise | Logical

-- | Internal

printSM :: SMInstruction -> Text
printSM (SPure i) = printSPure i
printSM (SIO   i) = printIO i <> "S"

printSPure :: SPureInstruction -> Text
printSPure (Unary    i  ) = printUnary i
printSPure (Indexed  i o) = toLowerShow o <> printIndexOperand i
printSPure (Binary   i  ) = toLowerShow i
printSPure (Binaries i  ) = printBinaries i
printSPure           i    = toLowerShow i

printBinaries :: (Foldable c, Functor c, Show i) => c i -> Text
printBinaries il = fmconcat $ toLowerShow <$> il

printUnary :: UnaryOperation -> Text
printUnary (UImmediate i o) = toLowerShow o <> "I " <> show i
printUnary             i    = toLowerShow i

printIndexOperand :: IndexOperand -> Text
printIndexOperand ITop           = ""
printIndexOperand (IImmediate i) = "I " <> show i
