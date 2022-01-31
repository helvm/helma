module HelVM.HelMA.Automaton.Types.StackType where

-- | Constructors
parseStackType :: String -> StackType
parseStackType raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "StackType '" <> toText raw <> "' is not valid StackType. Valid stackTypes are : " <> show stackTypes

defaultStackType :: StackType
defaultStackType = SeqStackType

stackTypes :: [StackType]
stackTypes = [ListStackType , SeqStackType , SListStackType]

-- | Types
data StackType = ListStackType | SeqStackType | SListStackType
  deriving stock (Eq , Read , Show)
