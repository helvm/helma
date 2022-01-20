module HelVM.HelMA.Automaton.Instruction.ALInstruction where

import           HelVM.HelMA.Automaton.Instruction.IOInstruction

-- | Constructors

blAnd :: Bool -> BinaryInstruction
blAnd False = BAnd
blAnd True  = LAnd

blOr :: Bool -> BinaryInstruction
blOr False = BOr
blOr True  = LOr

blXor :: Bool -> BinaryInstruction
blXor False = BXor
blXor True  = LXor

blEQ :: Bool -> BinaryInstruction
blEQ False = BEQ
blEQ True  = LEQ

blGT :: Bool -> BinaryInstruction
blGT False = BGT
blGT True  = LGT

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

data ALInstruction =
    Cons      Integer
  | Unary    !UnaryInstruction
  | Binary   !BinaryInstruction
  | Binaries [BinaryInstruction]
  | SStatic  !StackIndex !ManipulationInstruction
  | SDynamic             !ManipulationInstruction
  | SIO      !IOInstruction
  | Halibut
  | Pick
  | Dup
  | Rot
  | Swap
  | Discard
  deriving stock (Eq , Show , Read)

data UnaryInstruction = Neg | BNot | LNot
  deriving stock (Eq , Show , Read)

data BinaryInstruction =
     Add | Sub | Mul | Div | Mod
  | BAnd | BOr | BXor | BEQ | BGT
  | LAnd | LOr | LXor | LEQ | LGT
  deriving stock (Eq , Show , Read)

data ManipulationInstruction = Copy | Move | Slide
  deriving stock (Eq , Show , Read)

type StackIndex = Int
