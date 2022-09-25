module HelVM.HelMA.Automaton.Types.IntCellType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
defaultIntCellType :: IntCellType
defaultIntCellType = defaultEnum

intCellTypes :: [IntCellType]
intCellTypes = generateEnums 5

-- | Types
data IntCellType = IntegerType | Int8Type | Int16Type | Int32Type  | Int64Type
  deriving stock (Bounded , Enum , Eq , Read , Show)
