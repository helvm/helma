module HelVM.HelMA.Automata.ETA.Lexer where

import HelVM.HelMA.Automata.ETA.Token

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.Common.ReadText
import HelVM.Common.Util
import HelVM.HelMA.Automaton.WrapTokenList

-- Lexer

tokenize :: Source -> TokenList
tokenize = whiteTokenListToTokenList . unWrapTokenList . readTokens

readTokens :: Source -> WhiteTokens
readTokens source = (readText . toUppers) source :: WhiteTokens

type WhiteTokens = WrapTokenList WhiteTokenList
