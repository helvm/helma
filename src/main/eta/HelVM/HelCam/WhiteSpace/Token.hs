module HelVM.HelCam.WhiteSpace.Token where

import Data.Char

data Token =  S | T | N
  deriving (Eq, Ord, Enum, Show, Read)

type TokenList = [Token]

----

newtype WhiteToken = WhiteToken Token

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

----

whiteTokenToToken :: WhiteToken -> Token
whiteTokenToToken (WhiteToken token) = token

toBit :: (Num a) => Token -> a
toBit S = 0
toBit T = 1
toBit N = error $ show N

toBitChar :: Token -> Char
toBitChar = intToDigit . toBit
