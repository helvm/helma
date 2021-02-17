module HelVM.HelCam.Common.Types.RAMType where

data RAMType = ListRAMType | SeqRAMType | IntMapRAMType deriving (Eq, Read, Show)

ramTypes :: [RAMType]
ramTypes = [ListRAMType, SeqRAMType, IntMapRAMType]

defaultRAMType :: RAMType
defaultRAMType = IntMapRAMType

parseRAMType :: String -> RAMType
parseRAMType raw = valid $ readMaybe raw where
  valid (Just value)  = value
  valid Nothing = error $ "RAMType '" <> toText raw <> "' is not valid RAMType. Valid ramTypes are : " <> show ramTypes
