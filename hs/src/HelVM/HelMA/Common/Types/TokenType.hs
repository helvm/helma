module HelVM.HelMA.Common.Types.TokenType where

data TokenType = VisibleTokenType | WhiteTokenType | BothTokenType deriving (Eq , Read , Show)

tokenTypes :: [TokenType]
tokenTypes = [VisibleTokenType , WhiteTokenType , BothTokenType]

defaultTokenType :: TokenType
defaultTokenType = VisibleTokenType

parseTokenType :: Bool -> TokenType
parseTokenType True = VisibleTokenType
parseTokenType _    = WhiteTokenType
