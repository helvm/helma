module HelVM.HelMA.Automata.ETA.Lexer where

import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelIO.Extra
import           HelVM.HelIO.ReadText
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.WrapTokenList

-- | Lexer
tokenize :: Source -> TokenList
tokenize = whiteTokenListToTokenList . unWrapTokenList . readTokens

readTokens :: Source -> WhiteTokens
readTokens source = (readTextUnsafe . toUppers) source :: WhiteTokens

-- | Types
type WhiteTokens = WrapTokenList WhiteTokenList
