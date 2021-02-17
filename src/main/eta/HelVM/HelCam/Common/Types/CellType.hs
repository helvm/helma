module HelVM.HelCam.Common.Types.CellType where

data CellType = Int8Type | Word8Type deriving (Eq, Read, Show)

cellTypes :: [CellType]
cellTypes = [Int8Type, Word8Type]

defaultCellType :: CellType
defaultCellType = Word8Type

parseCellType :: String -> CellType
parseCellType raw = valid $ readMaybe raw where
  valid (Just value)  = value
  valid Nothing = error $ "CellType '" <> toText raw <> "' is not valid CellType. Valid cellTypes are : " <> show cellTypes
