module HelVM.HelMA.Automaton.Types.RAMType where

-- | Constructors
defaultRAMType :: RAMType
defaultRAMType = minBound

ramTypes :: NonEmpty RAMType
ramTypes = universeNonEmpty

-- | Types
data RAMType = MapListRAMType | SListRAMType | SeqRAMType | ListRAMType
  deriving stock (Bounded , Enum , Eq , Read , Show)
