module HelVM.HelMA.Automaton.Types.IntCellType where

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

-- | Constructors
parseIntCellType :: String -> IntCellType
parseIntCellType raw = fromJustWithText message $ readMaybe raw where
  message = "IntCellType '" <> toText raw <> "' is not valid IntCellType. Valid intCellTypes are : " <> show intCellTypes

defaultIntCellType :: IntCellType
defaultIntCellType = defaultEnum

intCellTypes :: [IntCellType]
intCellTypes = generateEnums 5

-- | Types
data IntCellType = IntegerType | Int8Type | Int16Type | Int32Type  | Int64Type
  deriving stock (Bounded , Enum , Eq , Read , Show)
