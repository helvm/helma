module HelVM.HelMA.Automata.WhiteSpace.Token where

import           HelVM.Common.Collections.SList
import           HelVM.Common.Digit.ToDigit
import           HelVM.Common.Safe

import           Text.Read

import qualified Text.Show

data Token =  S | T | N
  deriving stock (Eq , Ord , Enum , Show , Read)

type TokenList = [Token]
type TokenSList = SList Token

instance ToDigit Token where
  toDigit S = pure 0
  toDigit T = pure 1
  toDigit N = liftErrorTuple ("Wrong token" , show N)

----

newtype WhiteToken = WhiteToken { unWhiteToken :: Token}
  deriving stock (Eq)

instance Show WhiteToken where
  show (WhiteToken S) = " "
  show (WhiteToken T) = "\t"
  show (WhiteToken N) = "\n"

-- | Scanner
instance Read WhiteToken where
  readsPrec _ " "  = [( WhiteToken S , "")]
  readsPrec _ "\t" = [( WhiteToken T , "")]
  readsPrec _ "\n" = [( WhiteToken N , "")]
  readsPrec _ _    = []

type WhiteTokenList = [WhiteToken]

whiteTokenListToTokenList :: WhiteTokenList -> TokenList
whiteTokenListToTokenList = fmap unWhiteToken

