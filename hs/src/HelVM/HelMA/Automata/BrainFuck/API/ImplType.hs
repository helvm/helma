module HelVM.HelMA.Automata.BrainFuck.API.ImplType where

-- | Constructors
defaultImplType ∷ ImplType
defaultImplType = minBound

implTypes ∷ NonEmpty ImplType
implTypes = universeNonEmpty

-- | Type
data ImplType = FastType | TreeType | FlatType
  deriving stock (Bounded , Enum , Eq , Read , Show)
