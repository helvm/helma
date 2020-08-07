module HelVM.HelCam.Machines.WhiteSpace.Lexer where

import HelVM.HelCam.Machines.WhiteSpace.Token

import HelVM.HelCam.Common.Util

import Data.Maybe
import Text.Read

-- Lexer

tokenize :: String -> TokenList
tokenize =  tokenList . readTokens

readTokens :: String -> WhiteTokens
readTokens source = read source :: WhiteTokens

tokenList :: WhiteTokens -> TokenList
tokenList (WhiteTokens tokens) = map whiteTokenToToken tokens

-- WhiteTokens

newtype WhiteTokens = WhiteTokens WhiteTokenList deriving (Eq)

instance Show WhiteTokens where
  show (WhiteTokens tokens) = tokens >>= show

instance Read WhiteTokens where
  readsPrec _ source = [( WhiteTokens $ source >>= maybeToList . readMaybe . charToString, "")]
