module HelVM.HelCam.BrainFuck.Tokens where

import HelVM.HelCam.BrainFuck.Token

import Data.Maybe
import Text.Read

newtype Tokens = Tokens TokenList

--

instance Show Tokens where
  show (Tokens tokens) = tokens >>= show

charToString :: Char -> String
charToString = (:[])

instance Read Tokens where
  readsPrec _ text = [( Tokens $ text >>= maybeToList . readMaybe . charToString, "")]

--

tokenList :: Tokens -> TokenList
tokenList (Tokens tokens) = tokens

readTokens :: String -> Tokens
readTokens text = read text :: Tokens

tokenize :: String -> TokenList
tokenize =  tokenList . readTokens
