module HelVM.HelMA.Automaton.Types.TokenType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
parseTokenType:: String -> TokenType
parseTokenType raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid TokenType. Valid tokenTypes are : " <> show tokenTypes

defaultTokenType :: TokenType
defaultTokenType = defaultEnum

tokenTypes :: [TokenType]
tokenTypes = bothEnums

-- | Types
data TokenType = VisibleTokenType | WhiteTokenType
  deriving stock (Bounded , Enum , Eq , Read , Show)
