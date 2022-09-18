module HelVM.HelMA.Automaton.Types.RAMType where

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

-- | Constructors
parseRAMType :: String -> RAMType
parseRAMType raw = fromJustWithText message $ readMaybe raw where
  message = "RAMType '" <> toText raw <> "' is not valid RAMType. Valid ramTypes are : " <> show ramTypes

defaultRAMType :: RAMType
defaultRAMType = defaultEnum

ramTypes :: [RAMType]
ramTypes = generateEnums 4

-- | Types
data RAMType = MapListRAMType | SListRAMType | SeqRAMType | ListRAMType
  deriving stock (Bounded , Enum , Eq , Read , Show)
