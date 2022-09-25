module HelVM.HelMA.Automaton.Types.FormatType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
defaultFormatType :: FormatType
defaultFormatType = defaultEnum

formatTypes :: [FormatType]
formatTypes = bothEnums

-- | Types
data FormatType = BinaryLabel | TextLabel
  deriving stock (Bounded , Enum , Eq , Read , Show)
