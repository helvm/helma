module HelVM.HelCam.Machines.WhiteSpace.Token where

import Data.Char

data Token =  S | T | N
  deriving (Eq, Ord, Enum, Show, Read)

type TokenList = [Token]

toDigit :: (Num a) => Token -> a
toDigit S = 0
toDigit T = 1
toDigit N = error $ show N

toBitChar :: Token -> Char
toBitChar = intToDigit . toDigit

----

newtype WhiteToken = WhiteToken Token deriving (Eq)

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

whiteTokenToToken :: WhiteToken -> Token
whiteTokenToToken (WhiteToken token) = token

