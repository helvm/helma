module HelVM.HelMA.Automaton.Instruction.CFInstruction where

import           HelVM.HelIO.Collections.SList

-- | Constructors

cfJumpI :: CFInstruction
cfJumpI = Labeled Jump LTop

-- | Others

isNotJump :: Integral e => BranchTest -> e -> Bool
isNotJump t = not . isJump t

isJump :: Integral e => BranchTest -> e -> Bool
isJump NE  e = e /= 0
isJump EZ  e = e == 0
isJump LTZ e = e <  0
isJump GTZ e = e >  0

-- | Types
data CFInstruction =
    Mark     !Mark
  | Labeled      !LabeledOperation !LabelOperand
  | Return
  deriving stock (Eq , Read , Show)

data Mark  = MNatural Natural | MArtificial Label
  deriving stock (Eq , Read , Show)

data LabelOperand = LTop | LImmediate !Natural | LArtificial Label
  deriving stock (Eq , Read , Show)

type Label = SString --FIXME Artificial

data LabeledOperation = Call | Jump | Branch !BranchTest
  deriving stock (Eq , Read , Show)

data BranchTest = EZ | LTZ | GTZ | NE
  deriving stock (Eq , Read , Show)
