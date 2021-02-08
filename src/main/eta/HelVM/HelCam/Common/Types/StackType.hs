module HelVM.HelCam.Common.Types.StackType where

data StackType = ListStackType | SeqStackType deriving (Eq, Read, Show)

stackTypes :: [StackType]
stackTypes = [ListStackType, SeqStackType]

defaultStackType :: StackType
defaultStackType = SeqStackType

parseStackType :: String -> StackType
parseStackType raw = valid $ readMaybe raw where
  valid (Just value)  = value
  valid Nothing = error $ "StackType '" <> toText raw <> "' is not valid StackType. Valid stackTypes are : " <> show stackTypes
