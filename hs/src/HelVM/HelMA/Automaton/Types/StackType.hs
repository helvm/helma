module HelVM.HelMA.Automaton.Types.StackType where

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

-- | Constructors
parseStackType :: String -> StackType
parseStackType raw = fromJustWithText message $ readMaybe raw where
  message = "StackType '" <> toText raw <> "' is not valid StackType. Valid stackTypes are : " <> show stackTypes

defaultStackType :: StackType
defaultStackType = defaultEnum

stackTypes :: [StackType]
stackTypes = generateEnums 3

-- | Types
data StackType = SeqStackType | SListStackType | ListStackType
  deriving stock (Bounded , Enum , Eq , Read , Show)
