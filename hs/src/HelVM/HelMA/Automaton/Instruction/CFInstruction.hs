module HelVM.HelMA.Automaton.Instruction.CFInstruction where

import           HelVM.HelIO.Collections.SList

-- | Constructors

dJumpI :: CFInstruction
dJumpI = CDynamic Jump

-- | Others

isNotJump :: Integral e => BranchTest -> e -> Bool
isNotJump t = not . isJump t

isJump :: Integral e => BranchTest -> e -> Bool
isJump EZ  e = e == 0
isJump LTZ e = e <  0
isJump GTZ e = e >  0

-- | Types
data CFInstruction =
    SMark    !Label
  | DMark    !Natural
  | CStatic  !Label !LabelInstruction
  | CDynamic        !LabelInstruction
  | Return
  deriving stock (Eq , Read , Show)

type Label = SString

data LabelInstruction = Call | Jump | Branch !BranchTest
  deriving stock (Eq , Read , Show)

data BranchTest = EZ | LTZ | GTZ
   deriving stock (Eq , Read , Show)
