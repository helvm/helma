module HelVM.HelMA.Automaton.Types.RAMType where

-- | Constructors
parseRAMType :: String -> RAMType
parseRAMType raw = (valid . readMaybe) raw where
  valid (Just value) = value
  valid Nothing      = error $ "RAMType '" <> toText raw <> "' is not valid RAMType. Valid ramTypes are : " <> show ramTypes

defaultRAMType :: RAMType
defaultRAMType = MapListRAMType

ramTypes :: [RAMType]
ramTypes = [ListRAMType , SeqRAMType , SListRAMType , MapListRAMType]

-- | Types
data RAMType = ListRAMType | SeqRAMType | SListRAMType | MapListRAMType
  deriving stock (Eq , Read , Show)
