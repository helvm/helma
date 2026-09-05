module HelVM.HelMA.Automaton.API.LabelType where

-- | Constructors
defaultFormatType ∷ LabelType
defaultFormatType = minBound

formatTypes ∷ NonEmpty LabelType
formatTypes = universeNonEmpty

-- | Types
data LabelType
  = BinaryLabel
  | TextLabel
  deriving stock (Bounded, Enum, Eq, Read, Show)
