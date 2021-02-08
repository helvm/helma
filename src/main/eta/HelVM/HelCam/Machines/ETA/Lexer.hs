module HelVM.HelCam.Machines.ETA.Lexer where

import HelVM.HelCam.Machines.ETA.Token
import HelVM.HelCam.Common.Util

import Text.Read

import qualified Text.Show

-- Lexer

tokenize :: String -> TokenList
tokenize =  toTokenList . readTokens

readTokens :: String -> WhiteTokens
readTokens source = (read . toUppers) source :: WhiteTokens

toTokenList :: WhiteTokens -> TokenList
toTokenList (WhiteTokens tokens) = whiteTokenToToken <$> tokens

-- WhiteTokens

newtype WhiteTokens = WhiteTokens WhiteTokenList deriving (Eq)

instance Show WhiteTokens where
  show (WhiteTokens tokens) = show =<< tokens

instance Read WhiteTokens where
  readsPrec _ source = [( WhiteTokens $ maybeToList . readMaybe . one =<< source, "")]
