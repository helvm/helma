module HelVM.HelCam.WhiteSpace.Tokens where

import HelVM.HelCam.WhiteSpace.Tokens

import Data.Maybe
import Text.Read

newtype Tokens = Tokens TokenList

--

instance Show Tokens where
  show (Tokens tokens) = tokens >>= show

-- Lexer
instance Read Tokens where
  readsPrec _ source = [( Tokens $ source >>= maybeToList . readMaybe . charToString, "")]

--

tokenList :: Tokens -> TokenList
tokenList (Tokens tokens) = tokens

readTokens :: String -> Tokens
readTokens source = read source :: Tokens

tokenize :: String -> TokenList
tokenize =  tokenList . readTokens

