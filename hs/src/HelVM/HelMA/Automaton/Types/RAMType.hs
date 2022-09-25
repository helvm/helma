module HelVM.HelMA.Automaton.Types.RAMType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
defaultRAMType :: RAMType
defaultRAMType = defaultEnum

ramTypes :: [RAMType]
ramTypes = generateEnums 4

-- | Types
data RAMType = MapListRAMType | SListRAMType | SeqRAMType | ListRAMType
  deriving stock (Bounded , Enum , Eq , Read , Show)
