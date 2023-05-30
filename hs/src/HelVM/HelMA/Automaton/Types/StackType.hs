module HelVM.HelMA.Automaton.Types.StackType where

-- | Constructors
defaultStackType :: StackType
defaultStackType = minBound

stackTypes :: NonEmpty StackType
stackTypes = universeNonEmpty

-- | Types
data StackType = SeqStackType | SListStackType | ListStackType
  deriving stock (Bounded , Enum , Eq , Read , Show)
