module HelVM.HelMA.Automaton.Types.RAMType where

import           HelVM.HelIO.Extra

-- | Constructors
parseRAMType :: String -> RAMType
parseRAMType raw = fromJustWithText message $ readMaybe raw where
  message = "RAMType '" <> toText raw <> "' is not valid RAMType. Valid ramTypes are : " <> show ramTypes

defaultRAMType :: RAMType
defaultRAMType = MapListRAMType

ramTypes :: [RAMType]
ramTypes = [ListRAMType , SeqRAMType , SListRAMType , MapListRAMType]

-- | Types
data RAMType = ListRAMType | SeqRAMType | SListRAMType | MapListRAMType
  deriving stock (Eq , Read , Show)
