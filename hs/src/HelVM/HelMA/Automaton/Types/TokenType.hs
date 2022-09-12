module HelVM.HelMA.Automaton.Types.TokenType where

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

-- | Constructors
parseTokenType:: String -> TokenType
parseTokenType raw = fromJustWithText message $ readMaybe raw where
  message = "'" <> toText raw <> "' is not valid TokenType. Valid tokenTypes are : " <> show tokenTypes

defaultTokenType :: TokenType
defaultTokenType = defaultEnum

tokenTypes :: [TokenType]
tokenTypes = bothEnums

-- | Types
data TokenType = VisibleTokenType | WhiteTokenType
  deriving stock (Bounded , Enum , Eq , Read , Show)
