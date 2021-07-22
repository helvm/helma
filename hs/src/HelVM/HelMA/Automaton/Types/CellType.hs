module HelVM.HelMA.Automaton.Types.CellType where

data CellType = Int8Type | Word8Type | Int16Type | Word16Type | Int32Type | Word32Type | Int64Type | Word64Type
  deriving stock (Eq , Read , Show)

cellTypes :: [CellType]
cellTypes = [Int8Type , Word8Type , Int16Type , Word16Type , Int32Type , Word32Type , Int64Type , Word64Type]

defaultCellType :: CellType
defaultCellType = Word8Type

parseCellType :: String -> CellType
parseCellType raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "CellType '" <> toText raw <> "' is not valid CellType. Valid cellTypes are : " <> show cellTypes
