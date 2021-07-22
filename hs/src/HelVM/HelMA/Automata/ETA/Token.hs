module HelVM.HelMA.Automata.ETA.Token where

import           HelVM.Common.Digit.ToDigit
import           HelVM.Common.Safe

import           Data.Vector                as Vector

import qualified Text.Read
import qualified Text.Show

data Token = E | T | A | O | I | N | S | H | R
  deriving stock (Eq , Ord , Enum , Show , Read)

type TokenList   = [Token]
type TokenVector = Vector Token

instance ToDigit Token where
  toDigit H = pure 0
  toDigit T = pure 1
  toDigit A = pure 2
  toDigit O = pure 3
  toDigit I = pure 4
  toDigit N = pure 5
  toDigit S = pure 6
  toDigit E = liftErrorTuple ("Wrong token" , show E)
  toDigit R = liftErrorTuple ("Wrong token" , show R)

----

newtype WhiteToken = WhiteToken { unWhiteToken :: Token}
  deriving stock (Eq)

type WhiteTokenList = [WhiteToken]

instance Show WhiteToken where
  show (WhiteToken R) = "\n"
  show (WhiteToken t) = show t

-- | Scanner
instance Read WhiteToken where
  readsPrec _ "\n" = [( WhiteToken R , "")]
  readsPrec _ "E"  = [( WhiteToken E , "")]
  readsPrec _ "T"  = [( WhiteToken T , "")]
  readsPrec _ "A"  = [( WhiteToken A , "")]
  readsPrec _ "O"  = [( WhiteToken O , "")]
  readsPrec _ "I"  = [( WhiteToken I , "")]
  readsPrec _ "N"  = [( WhiteToken N , "")]
  readsPrec _ "S"  = [( WhiteToken S , "")]
  readsPrec _ "H"  = [( WhiteToken H , "")]
  readsPrec _ _    = []

tokenToWhiteTokenPair :: Token -> (WhiteToken , String)
tokenToWhiteTokenPair t = (WhiteToken t , "")

whiteTokenListToTokenList :: WhiteTokenList -> TokenList
whiteTokenListToTokenList = fmap unWhiteToken
