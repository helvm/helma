module HelVM.HelCam.Machines.ETA.Lexer where

import HelVM.HelCam.Machines.ETA.Token
import HelVM.HelCam.Common.Util

import Data.Maybe
import Text.Read

-- Lexer

tokenize :: String -> TokenList
tokenize =  toTokenList . readTokens

readTokens :: String -> WhiteTokens
readTokens source = (read . toUppers) source :: WhiteTokens

toTokenList :: WhiteTokens -> TokenList
toTokenList (WhiteTokens tokens) = map whiteTokenToToken tokens

-- WhiteTokens

newtype WhiteTokens = WhiteTokens WhiteTokenList deriving (Eq)

instance Show WhiteTokens where
  show (WhiteTokens tokens) = tokens >>= show

instance Read WhiteTokens where
  readsPrec _ source = [( WhiteTokens $ source >>= maybeToList . readMaybe . charToString, "")]
