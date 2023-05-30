module HelVM.HelMA.Automata.BrainFuck.API.BFType where

-- | Constructors
defaultBFType :: BFType
defaultBFType = minBound

bfTypes :: NonEmpty BFType
bfTypes = universeNonEmpty

-- | Type
data BFType = FastType | TreeType | FlatType
  deriving stock (Bounded , Enum , Eq , Read , Show)
