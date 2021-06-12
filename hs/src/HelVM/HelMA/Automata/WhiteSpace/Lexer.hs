module HelVM.HelMA.Automata.WhiteSpace.Lexer where

import HelVM.HelMA.Automata.WhiteSpace.Token

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.Common.ReadText
import HelVM.HelMA.Automaton.Types.TokenType
import HelVM.HelMA.Automaton.WrapTokenList

-- Lexer

tokenize :: TokenType -> Source -> TokenList
tokenize VisibleTokenType = tokenizeVisible
tokenize _                = tokenizeWhite

tokenizeVisible :: Source -> TokenList
tokenizeVisible = unWrapTokenList . readVisibleTokens

tokenizeWhite :: Source -> TokenList
tokenizeWhite = whiteTokenListToTokenList . unWrapTokenList . readWhiteTokens

readVisibleTokens :: Source -> VisibleTokens
readVisibleTokens source = readText source :: VisibleTokens

readWhiteTokens :: Source -> WhiteTokens
readWhiteTokens source = readText source :: WhiteTokens

type VisibleTokens = WrapTokenList TokenList

type WhiteTokens = WrapTokenList WhiteTokenList
