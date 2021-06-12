module HelVM.HelMA.Automata.WhiteSpace.Token where

import HelVM.Common.Safe
import HelVM.Common.Digit.ToDigit

import Text.Read

import qualified Text.Show

data Token =  S | T | N
  deriving stock (Eq , Ord , Enum , Show , Read)

type TokenList = [Token]

instance ToDigit Token where
  toDigit S = safe 0
  toDigit T = safe 1
  toDigit N = safeErrorTuple ("Wrong token" , show N)

----

newtype WhiteToken = WhiteToken Token deriving stock (Eq)

instance Show WhiteToken where
  show (WhiteToken S) = " "
  show (WhiteToken T) = "\t"
  show (WhiteToken N) = "\n"

-- Scanner
instance Read WhiteToken where
  readsPrec _ " "  = [( WhiteToken S , "")]
  readsPrec _ "\t" = [( WhiteToken T , "")]
  readsPrec _ "\n" = [( WhiteToken N , "")]
  readsPrec _ _    = []

type WhiteTokenList = [WhiteToken]

whiteTokenListToTokenList :: WhiteTokenList -> TokenList
whiteTokenListToTokenList = fmap whiteTokenToToken

whiteTokenToToken :: WhiteToken -> Token
whiteTokenToToken (WhiteToken token) = token
