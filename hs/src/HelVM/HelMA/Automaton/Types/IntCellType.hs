module HelVM.HelMA.Automaton.Types.IntCellType where

data IntCellType = Int8Type | Int16Type | Int32Type  | Int64Type | IntegerType
  deriving stock (Eq , Read , Show)

intCellTypes :: [IntCellType]
intCellTypes = [Int8Type , Int16Type , Int32Type , Int64Type , IntegerType]

defaultIntCellType :: IntCellType
defaultIntCellType = IntegerType

parseIntCellType :: String -> IntCellType
parseIntCellType raw = valid $ readMaybe raw where
  valid (Just value)  = value
  valid Nothing = error $ "IntCellType '" <> toText raw <> "' is not valid IntCellType. Valid intCellTypes are : " <> show intCellTypes
