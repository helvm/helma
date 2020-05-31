module HelVM.HelCam.BrainFuck.Tokens where

import HelVM.HelCam.BrainFuck.Token
import HelVM.HelCam.Common.Util

import Data.Maybe
import Text.Read

newtype Tokens = Tokens TokenList

--

instance Show Tokens where
  show (Tokens tokens) = tokens >>= show

instance Read Tokens where
  readsPrec _ source = [( Tokens $ source >>= maybeToList . readMaybe . charToString, "")]

--

tokenList :: Tokens -> TokenList
tokenList (Tokens tokens) = tokens

readTokens :: String -> Tokens
readTokens source = read source :: Tokens

-- Lexer
tokenize :: String -> TokenList
tokenize =  tokenList . readTokens
