module HelVM.HelMA.Automaton.Types.TokenType where

-- | Constructors
parseTokenType :: Bool -> TokenType
parseTokenType True = VisibleTokenType
parseTokenType _    = WhiteTokenType

defaultTokenType :: TokenType
defaultTokenType = VisibleTokenType

tokenTypes :: [TokenType]
tokenTypes = [VisibleTokenType , WhiteTokenType , BothTokenType]

-- | Types
data TokenType = VisibleTokenType | WhiteTokenType | BothTokenType
  deriving stock (Eq , Read , Show)
