module HelVM.HelMA.Automaton.Types.CellType where

-- | Constructors
defaultCellType :: CellType
defaultCellType = minBound

cellTypes :: NonEmpty CellType
cellTypes = universeNonEmpty

-- | Types
data CellType = Int8Type | Word8Type | Int16Type | Word16Type | Int32Type | Word32Type | Int64Type | Word64Type
  deriving stock (Bounded , Enum , Eq , Read , Show)
