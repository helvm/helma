module HelVM.HelMA.Automata.BrainFuck.Lexer where

import HelVM.HelMA.Automata.BrainFuck.Token

import HelVM.HelMA.Common.WrapTokenList

import qualified Text.Read as Read

-- Lexer
tokenize :: String -> TokenList
tokenize =  unWrapTokenList . readTokens

readTokens :: String -> Tokens
readTokens source = Read.read source :: Tokens

type Tokens = WrapTokenList TokenList
