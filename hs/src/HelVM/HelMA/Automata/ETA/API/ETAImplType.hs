module HelVM.HelMA.Automata.ETA.API.ETAImplType where

-- | Constructors
defaultETAImplType :: ETAImplType
defaultETAImplType = minBound

etaImplTypes:: NonEmpty ETAImplType
etaImplTypes = universeNonEmpty

-- | Type
data ETAImplType = Fast | Original
  deriving stock (Bounded , Enum , Eq , Read , Show)
