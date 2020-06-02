module HelVM.HelCam.BrainFuck.Lexer where

import HelVM.HelCam.BrainFuck.Token
import HelVM.HelCam.Common.Util

import Data.Maybe
import Text.Read

-- Lexer
tokenizeBF :: String -> TokenList
tokenizeBF =  tokenList . readTokens

readTokens :: String -> Tokens
readTokens source = read source :: Tokens

tokenList :: Tokens -> TokenList
tokenList (Tokens tokens) = tokens

----

newtype Tokens = Tokens TokenList

instance Show Tokens where
  show (Tokens tokens) = tokens >>= show

instance Read Tokens where
  readsPrec _ source = [( Tokens $ source >>= maybeToList . readMaybe . charToString, "")]
