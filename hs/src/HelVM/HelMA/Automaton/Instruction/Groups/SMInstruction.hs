module HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction where

import           Data.Bits                                              ( Bits, complement, (.&.), (.|.) )

import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction

import           Prettyprinter                                          ( Pretty (pretty), (<+>) )
import qualified Prettyprinter                                          as PP

-- | Constructors

blAnd ∷ OperatorType → BinaryOperation
blAnd Bitwise = BAnd
blAnd Logical = LAnd

blOr ∷ OperatorType → BinaryOperation
blOr Bitwise = BOr
blOr Logical = LOr

blXor ∷ OperatorType → BinaryOperation
blXor Bitwise = BXor
blXor Logical = LXor

blEQ ∷ OperatorType → BinaryOperation
blEQ Bitwise = BEQ
blEQ Logical = LEQ

blGT ∷ OperatorType → BinaryOperation
blGT Bitwise = BGT
blGT Logical = LGT

-- | Unary Evaluation

calculateUnary ∷ (Integral a, Bits a) ⇒ a → UnaryOperation → a
calculateUnary operand operation = doUnary operation operand

doUnary ∷ (Integral a, Bits a) ⇒ UnaryOperation → a → a
doUnary Neg             a  = -a
doUnary BNot            a  = complement a
doUnary LNot            a  = fromBool (not $ toBool a)
doUnary (UImmediate i o) a = doBinary o (fromIntegral i) a

-- | Binary Evaluation

calculateOps ∷ (Integral a, Bits a) ⇒ a → a → [BinaryOperation] → [a]
calculateOps operand operand' = map (calculateOp operand operand')

calculateOp ∷ (Integral a, Bits a) ⇒ a → a → BinaryOperation → a
calculateOp operand operand' operation = doBinary operation operand' operand

doBinary ∷ (Integral a, Bits a) ⇒ BinaryOperation → a → a → a
doBinary Add  a b = a + b
doBinary Sub  a b = a - b
doBinary Mul  a b = a * b
doBinary Div  a b = a `div` b
doBinary Mod  a b = a `mod` b

-- Bitwise operations
doBinary BAnd a b = a .&. b
doBinary BOr  a b = a .|. b
doBinary BXor a b = a `xor` b
doBinary BEQ  a b = complement (a `xor` b)
doBinary BGT  a b = a .&. complement b

-- Logical operations
doBinary LAnd a b = fromBool (toBool a && toBool b)
doBinary LOr  a b = fromBool (toBool a || toBool b)
doBinary LXor a b = fromBool (toBool a /= toBool b)
doBinary LEQ  a b = fromBool (a == b)
doBinary LGT  a b = lGT a b

lGT ∷ (Integral a) ⇒ a → a → a
lGT a b = fromBool $ a > b

fromBool ∷ Integral a ⇒ Bool → a
fromBool False = 0
fromBool True  = 1

toBool ∷ Integral a ⇒ a → Bool
toBool a = a /= 0

-- | Types

data SMInstruction
  = SPure !SPureInstruction
  | SIO !IOInstruction
  deriving stock (Eq, Read, Show)

data SPureInstruction
  = Cons !Integer
  | Unary !UnaryOperation
  | Binary !BinaryOperation
  | Binaries [BinaryOperation]
  | Indexed !IndexOperand !IndexedOperation
  | Halibut
  | Pick
  | Roll
  | Discard
  deriving stock (Eq, Read, Show)

data IndexOperand
  = ITop
  | IImmediate !ImmediateIndex
  deriving stock (Eq, Read, Show)

data UnaryOperation
  = Neg
  | BNot
  | LNot
  | UImmediate Integer BinaryOperation
  deriving stock (Eq, Read, Show)

data BinaryOperation
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | BAnd
  | BOr
  | BXor
  | BEQ
  | BGT
  | LAnd
  | LOr
  | LXor
  | LEQ
  | LGT
  deriving stock (Eq, Read, Show)

data IndexedOperation
  = Copy
  | Move
  | Slide
  deriving stock (Eq, Read, Show)

data OperatorType
  = Bitwise
  | Logical

-- | Pretty instances

instance Pretty SMInstruction where
  pretty (SPure i) = pretty i
  pretty (SIO i)   = pretty i <> "S"

instance Pretty SPureInstruction where
  pretty (Unary i)      = pretty i
  pretty (Binary i)     = pretty (toLowerShow i)
  pretty (Binaries ops)=PP.hcat (pretty . toLowerShow <$> ops)
  pretty (Indexed i o)  = pretty (toLowerShow o) <+> pretty i
  pretty i              = pretty (toLowerShow i)

instance Pretty UnaryOperation where
  pretty (UImmediate i o) = pretty (toLowerShow o) <> "I" <+> pretty i
  pretty i                = pretty (toLowerShow i)

instance Pretty IndexOperand where
  pretty ITop           = PP.emptyDoc
  pretty (IImmediate i) = "I" <+> pretty i
