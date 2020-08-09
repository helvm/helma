module HelVM.HelCam.Machines.BrainFuck.Lexer where

import HelVM.HelCam.Machines.BrainFuck.Token

import HelVM.HelCam.Common.Util

import Data.Maybe
import Text.Read

-- Lexer
tokenize :: String -> TokenList
tokenize =  tokenList . readTokens

readTokens :: String -> Tokens
readTokens source = read source :: Tokens

tokenList :: Tokens -> TokenList
tokenList (Tokens tokens) = tokens

----

newtype Tokens = Tokens TokenList

instance Show Tokens where
  show (Tokens tokens) = show =<< tokens

instance Read Tokens where
  readsPrec _ source = [( Tokens $ maybeToList . readMaybe . charToString =<< source, "")]
