module HelVM.HelMA.Automaton.Types.TokenType where

import           HelVM.HelIO.SwitchEnum

-- | Constructors
defaultTokenType :: TokenType
defaultTokenType = defaultEnum

tokenTypes :: [TokenType]
tokenTypes = bothEnums

-- | Types
data TokenType = WhiteTokenType | VisibleTokenType
  deriving stock (Bounded , Enum , Eq , Read , Show)
