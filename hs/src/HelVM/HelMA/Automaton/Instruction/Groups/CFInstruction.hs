module HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.TextExtra

import           HelVM.HelIO.Collections.SList

import           Prettyprinter                                      ( Pretty (pretty), (<+>) )
import qualified Prettyprinter                                      as PP

-- | Others

isNotJump ∷ Integral e ⇒ BranchTest → e → Bool
isNotJump t = not . isJump t

isJump ∷ Integral e ⇒ BranchTest → e → Bool
isJump NE  e = e /= 0
isJump EZ  e = e == 0
isJump LTZ e = e <  0
isJump GTZ e = e >  0

-- | Types

data CFInstruction
  = Mark !Mark
  | Branch !BranchOperand !BranchTest
  | Labeled !LabelOperand !LabelOperation
  | Switch !(NonEmpty Label)
  | Return
  deriving stock (Eq, Read, Show)

data Mark
  = MNatural Natural
  | MArtificial Label
  deriving stock (Eq, Read, Show)

data LabelOperand
  = LTop
  | LImmediate !Natural
  | LArtificial Label
  deriving stock (Eq, Read, Show)

data BranchOperand
  = BSwapped
  | BTop
  | BImmediate !Natural
  | BArtificial Label
  deriving stock (Eq, Read, Show)

--FIXME
--data Artificial = Integer | Label
--

type Label = SString --FIXME Artificial

data LabelOperation
  = Call
  | Jump
  deriving stock (Eq, Read, Show)

data BranchTest
  = EZ
  | LTZ
  | GTZ
  | NE
  deriving stock (Eq, Read, Show)

-- | Pretty instances

instance Pretty CFInstruction where
  pretty (Mark i)      = PP.line <> "mark" <> pretty i
  pretty (Branch i t)  = pretty t <> pretty i
  pretty (Labeled i o) = pretty (toLowerShow o) <> pretty i
  pretty (Switch ls)   = "switch" <+> PP.hsep (PP.viaShow <$> toList ls)
  pretty Return        = pretty (toLowerShow Return)

instance Pretty Mark where
  pretty (MNatural i)    = "M" <+> pretty i
  pretty (MArtificial i) = "A" <+> PP.viaShow i

instance Pretty LabelOperand where
  pretty LTop            = PP.emptyDoc
  pretty (LImmediate i)  = "I" <+> pretty i
  pretty (LArtificial i) = "A" <+> PP.viaShow i

instance Pretty BranchOperand where
  pretty BTop            = PP.emptyDoc
  pretty BSwapped        = "S"
  pretty (BImmediate i)  = "I" <+> pretty i
  pretty (BArtificial i) = "A" <+> PP.viaShow i

instance Pretty BranchTest where
  pretty t = "b" <> PP.viaShow t
