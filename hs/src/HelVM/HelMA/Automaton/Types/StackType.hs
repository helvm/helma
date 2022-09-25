module HelVM.HelMA.Automaton.Types.StackType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
defaultStackType :: StackType
defaultStackType = defaultEnum

stackTypes :: [StackType]
stackTypes = generateEnums 3

-- | Types
data StackType = SeqStackType | SListStackType | ListStackType
  deriving stock (Bounded , Enum , Eq , Read , Show)
