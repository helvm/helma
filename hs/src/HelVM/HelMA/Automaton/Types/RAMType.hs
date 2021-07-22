module HelVM.HelMA.Automaton.Types.RAMType where

data RAMType = ListRAMType | SeqRAMType | SListRAMType | MapListRAMType
  deriving stock (Eq , Read , Show)

ramTypes :: [RAMType]
ramTypes = [ListRAMType , SeqRAMType , SListRAMType , MapListRAMType]

defaultRAMType :: RAMType
defaultRAMType = MapListRAMType

parseRAMType :: String -> RAMType
parseRAMType raw = (valid . readMaybe) raw where
  valid (Just value) = value
  valid Nothing      = error $ "RAMType '" <> toText raw <> "' is not valid RAMType. Valid ramTypes are : " <> show ramTypes
