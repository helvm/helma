module HelVM.HelMA.Automaton.API.LabelType where

-- | Constructors
defaultLabelType ∷ LabelType
defaultLabelType = minBound

labelTypes ∷ NonEmpty LabelType
labelTypes = universeNonEmpty

-- | Types
data LabelType
  = BinaryLabel
  | TextLabel
  deriving stock (Bounded, Enum, Eq, Read, Show)
