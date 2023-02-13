module HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.TextExtra

import           HelVM.HelIO.Collections.SList

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
  | Branch  !BranchOperand !BranchTest
  | Labeled !LabelOperand !LabelOperation
  | Return
  deriving stock (Eq , Read , Show)

data Mark  = MNatural Natural | MArtificial Label
  deriving stock (Eq , Read , Show)

data LabelOperand = LTop | LImmediate !Natural | LArtificial Label
  deriving stock (Eq , Read , Show)

data BranchOperand = BSwapped | BTop | BImmediate !Natural | BArtificial Label
  deriving stock (Eq , Read , Show)

type Label = SString --FIXME Artificial

data LabelOperation = Call | Jump
  deriving stock (Eq , Read , Show)

data BranchTest = EZ | LTZ | GTZ | NE
  deriving stock (Eq , Read , Show)

-- | Internal

printCF :: CFInstruction -> Text
printCF (Mark     i  ) = "\nmark" <> printMark i
printCF (Branch i t)   = printBranchTest t <> printBranchOperand i
printCF (Labeled  i o) = toLowerShow o <> printLabelOperand i
printCF           i    = toLowerShow i

printMark :: Mark -> Text
printMark (MNatural    i) = "M " <> show i
printMark (MArtificial i) = "A " <> show i

printBranchTest :: BranchTest -> Text
printBranchTest t = "b" <> show t

printBranchOperand :: BranchOperand -> Text
printBranchOperand  BTop           = ""
printBranchOperand  BSwapped       = "S"
printBranchOperand (BImmediate  i) = "I " <> show i
printBranchOperand (BArtificial i) = "A " <> show i

printLabelOperand :: LabelOperand -> Text
printLabelOperand  LTop           = ""
printLabelOperand (LImmediate  i) = "I " <> show i
printLabelOperand (LArtificial i) = "A " <> show i
