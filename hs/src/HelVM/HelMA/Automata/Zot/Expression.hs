module HelVM.HelMA.Automata.Zot.Expression where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Containers.Util
import           HelVM.HelIO.Digit.Digitable
import           HelVM.HelIO.Digit.ToDigit

import           Control.Monad.Writer.Lazy

import qualified Data.DList                  as D

import           Text.Read
import qualified Text.Show

showExpressionList :: ExpressionList -> Text
showExpressionList = showFoldable

readExpressionList :: Text -> ExpressionList
readExpressionList = stringToExpressionList . toString

stringToExpressionList :: String -> ExpressionList
stringToExpressionList s = charToExpressionList =<< s

charToExpressionList :: Char -> ExpressionList
charToExpressionList = maybeToList . rightToMaybe . charToExpressionSafe

charToExpression :: Char -> Expression
charToExpression = unsafe . charToExpressionSafe

charToExpressionSafe :: MonadSafe m => Char -> m Expression
charToExpressionSafe '0' = pure Zero
charToExpressionSafe '1' = pure One
charToExpressionSafe  c  = liftErrorWithPrefix "charToExpression" $ one c

-- | Types
type ExpressionDList = D.DList Expression

type ExpressionList = [Expression]

data Expression = Zero | One | Expression (Expression -> Out Expression)

type Out = Writer ExpressionDList

instance Read Expression where
  readsPrec _ []      = []
  readsPrec _ (c : s) = [(charToExpression c , s)]
  readList s = [(stringToExpressionList s , "")]

instance Show Expression where
  show Zero           = "0"
  show One            = "1"
  show (Expression _) = "function"
  showList fs  = (concatMap show fs <>)

instance Digitable Expression where
  fromDigit 0 = pure Zero
  fromDigit 1 = pure One
  fromDigit t = wrongToken t

instance ToDigit Expression where
  toDigit Zero = pure 0
  toDigit One  = pure 1
  toDigit t    = wrongToken t
