module HelVM.HelMA.Automaton.Types.StackType where

import           HelVM.HelIO.Extra

-- | Constructors
parseStackType :: String -> StackType
parseStackType raw = fromJustWithText message $ readMaybe raw where
  message = "StackType '" <> toText raw <> "' is not valid StackType. Valid stackTypes are : " <> show stackTypes

defaultStackType :: StackType
defaultStackType = SeqStackType

stackTypes :: [StackType]
stackTypes = [ListStackType , SeqStackType , SListStackType]

-- | Types
data StackType = ListStackType | SeqStackType | SListStackType
  deriving stock (Eq , Read , Show)
