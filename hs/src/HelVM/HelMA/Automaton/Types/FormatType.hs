module HelVM.HelMA.Automaton.Types.FormatType where

-- | Constructors
defaultFormatType :: FormatType
defaultFormatType = minBound

formatTypes :: NonEmpty FormatType
formatTypes = universeNonEmpty

-- | Types
data FormatType = BinaryLabel | TextLabel
  deriving stock (Bounded , Enum , Eq , Read , Show)
