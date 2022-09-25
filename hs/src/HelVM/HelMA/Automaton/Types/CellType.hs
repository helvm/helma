module HelVM.HelMA.Automaton.Types.CellType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
defaultCellType :: CellType
defaultCellType = defaultEnum

cellTypes :: [CellType]
cellTypes = generateEnums 8

-- | Types
data CellType = Int8Type | Word8Type | Int16Type | Word16Type | Int32Type | Word32Type | Int64Type | Word64Type
  deriving stock (Bounded , Enum , Eq , Read , Show)
