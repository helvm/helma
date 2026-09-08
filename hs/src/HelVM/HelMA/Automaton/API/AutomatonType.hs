module HelVM.HelMA.Automaton.API.AutomatonType where

-- | Constructors
defaultAutomatonType ∷ AutomatonType
defaultAutomatonType = minBound

automatonTypes∷ NonEmpty AutomatonType
automatonTypes = universeNonEmpty

-- | Type
data AutomatonType
  = Common
  | Custom
  deriving stock (Bounded, Enum, Eq, Read, Show)
