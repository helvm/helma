module HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction

import           Prettyprinter                                          ( Pretty (pretty), (<+>) )

-- | Types

data LSInstruction
  = Load --Restore --Fetch
  | LoadD !ImmediateIndex
  | Store --Save
  | RStore --Save
  | StoreI Integer
  | RStoreD ImmediateIndex
  | StoreID !Integer !ImmediateIndex
  | MoveD !ImmediateIndex !ImmediateIndex
  | MIO !IOInstruction
  deriving stock (Eq, Read, Show)

-- | Pretty instance

instance Pretty LSInstruction where
  pretty Load            = pretty (toLowerShow Load)
  pretty (LoadD i)       = "loadD" <+> pretty i
  pretty Store           = pretty (toLowerShow Store)
  pretty RStore          = pretty (toLowerShow RStore)
  pretty (StoreI i)      = "storeI" <+> pretty i
  pretty (RStoreD i)     = "rStoreD" <+> pretty i
  pretty (StoreID v i)   = "storeID" <+> pretty v <+> pretty i
  pretty (MoveD src dst) = "moveD" <+> pretty src <+> pretty dst
  pretty (MIO i)         = pretty i

