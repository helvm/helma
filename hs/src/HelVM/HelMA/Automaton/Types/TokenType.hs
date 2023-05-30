module HelVM.HelMA.Automaton.Types.TokenType where

-- | Constructors
defaultTokenType :: TokenType
defaultTokenType = minBound

tokenTypes :: NonEmpty TokenType
tokenTypes = universeNonEmpty

-- | Types
data TokenType = WhiteTokenType | VisibleTokenType
  deriving stock (Bounded , Enum , Eq , Read , Show)
