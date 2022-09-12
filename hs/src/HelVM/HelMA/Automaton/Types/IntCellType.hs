module HelVM.HelMA.Automaton.Types.IntCellType where

import           HelVM.HelIO.Extra

-- | Constructors
parseIntCellType :: String -> IntCellType
parseIntCellType raw = fromJustWithText message $ readMaybe raw where
  message = "IntCellType '" <> toText raw <> "' is not valid IntCellType. Valid intCellTypes are : " <> show intCellTypes

defaultIntCellType :: IntCellType
defaultIntCellType = IntegerType

intCellTypes :: [IntCellType]
intCellTypes = [Int8Type , Int16Type , Int32Type , Int64Type , IntegerType]

-- | Types
data IntCellType = Int8Type | Int16Type | Int32Type  | Int64Type | IntegerType
  deriving stock (Bounded , Enum , Eq , Read , Show)
