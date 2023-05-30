module HelVM.HelMA.Automaton.Types.IntCellType where

-- | Constructors
defaultIntCellType :: IntCellType
defaultIntCellType = minBound

intCellTypes :: NonEmpty IntCellType
intCellTypes = universeNonEmpty

-- | Types
data IntCellType = IntegerType | Int8Type | Int16Type | Int32Type  | Int64Type
  deriving stock (Bounded , Enum , Eq , Read , Show)
