module HelVM.HelMA.Automata.ETA.Token where

import HelVM.Common.Digit.ToDigit
import HelVM.Common.Safe

import qualified Text.Read
import qualified Text.Show

data Token = E | T | A | O | I | N | S | H | R
  deriving stock (Eq , Ord , Enum , Show , Read)

type TokenList = [Token]

instance ToDigit Token where
  toDigit H = safe 0
  toDigit T = safe 1
  toDigit A = safe 2
  toDigit O = safe 3
  toDigit I = safe 4
  toDigit N = safe 5
  toDigit S = safe 6
  toDigit E = safeErrorTuple ("Wrong token" , show E)
  toDigit R = safeErrorTuple ("Wrong token" , show R)

----

newtype WhiteToken = WhiteToken Token deriving stock (Eq)

type WhiteTokenList = [WhiteToken]

instance Show WhiteToken where
  show (WhiteToken R) = "\n"
  show (WhiteToken t) = show t

-- Scanner
instance Read WhiteToken where
  readsPrec _ "\n" = [( WhiteToken R , "")]
  readsPrec _ "E" = [( WhiteToken E , "")]
  readsPrec _ "T" = [( WhiteToken T , "")]
  readsPrec _ "A" = [( WhiteToken A , "")]
  readsPrec _ "O" = [( WhiteToken O , "")]
  readsPrec _ "I" = [( WhiteToken I , "")]
  readsPrec _ "N" = [( WhiteToken N , "")]
  readsPrec _ "S" = [( WhiteToken S , "")]
  readsPrec _ "H" = [( WhiteToken H , "")]
  readsPrec _ _   = []

tokenToWhiteTokenPair :: Token -> (WhiteToken , String)
tokenToWhiteTokenPair t = (WhiteToken t , "")

whiteTokenListToTokenList :: WhiteTokenList -> TokenList
whiteTokenListToTokenList = fmap whiteTokenToToken

whiteTokenToToken :: WhiteToken -> Token
whiteTokenToToken (WhiteToken token) = token
