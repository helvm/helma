module HelVM.HelCam.Machines.ETA.Token where

import Text.Read

import qualified Text.Show

data Token = E | T | A | O | I | N | S | H | R
  deriving (Eq, Ord, Enum, Show, Read)

type TokenList = [Token]

toDigit :: (Num a) => Token -> a
toDigit H = 0
toDigit T = 1
toDigit A = 2
toDigit O = 3
toDigit I = 4
toDigit N = 5
toDigit S = 6
toDigit E = error $ show E
toDigit R = error $ show R

toIsString :: Maybe Token -> String
toIsString (Just t) = show t
toIsString Nothing = ""

----

newtype WhiteToken = WhiteToken Token deriving (Eq)

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

tokenToWhiteTokenPair :: Token -> (WhiteToken, String)
tokenToWhiteTokenPair t = (WhiteToken t, "")

whiteTokenToToken :: WhiteToken -> Token
whiteTokenToToken (WhiteToken token) = token
