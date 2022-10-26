module HelVM.HelMA.Automata.BrainFuck.API.BFType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
defaultBFType :: BFType
defaultBFType = defaultEnum

bfTypes :: [BFType]
bfTypes = generateEnums 3

data BFType = FastType | TreeType | FlatType
  deriving stock (Bounded , Enum , Eq , Read , Show)
