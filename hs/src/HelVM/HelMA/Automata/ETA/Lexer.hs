module HelVM.HelMA.Automata.ETA.Lexer where

import HelVM.HelMA.Automata.ETA.Token
import HelVM.HelMA.Common.Util

import HelVM.HelMA.Common.WrapTokenList

import qualified Text.Read as Read

-- Lexer

tokenize :: String -> TokenList
tokenize = whiteTokenListToTokenList . unWrapTokenList . readTokens

readTokens :: String -> WhiteTokens
readTokens source = (Read.read . toUppers) source :: WhiteTokens

type WhiteTokens = WrapTokenList WhiteTokenList
