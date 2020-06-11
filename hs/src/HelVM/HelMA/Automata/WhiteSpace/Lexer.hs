module HelVM.HelMA.Automata.WhiteSpace.Lexer where

import HelVM.HelMA.Automata.WhiteSpace.Token

import HelVM.HelMA.Common.Types.TokenType
import HelVM.HelMA.Common.WrapTokenList

import qualified Text.Read as Read

-- Lexer

tokenize :: TokenType -> String -> TokenList
tokenize VisibleTokenType = tokenizeVisible
tokenize _                = tokenizeWhite

tokenizeVisible :: String -> TokenList
tokenizeVisible = unWrapTokenList . readVisibleTokens

tokenizeWhite :: String -> TokenList
tokenizeWhite = whiteTokenListToTokenList . unWrapTokenList . readWhiteTokens

readVisibleTokens :: String -> VisibleTokens
readVisibleTokens source = Read.read source :: VisibleTokens

readWhiteTokens :: String -> WhiteTokens
readWhiteTokens source = Read.read source :: WhiteTokens

type VisibleTokens = WrapTokenList TokenList

type WhiteTokens = WrapTokenList WhiteTokenList
