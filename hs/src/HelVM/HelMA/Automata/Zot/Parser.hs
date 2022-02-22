module HelVM.HelMA.Automata.Zot.Parser (
  parse,
) where

import           HelVM.HelMA.Automata.Zot.Expression

import qualified Data.Text                           as Text

parse :: Text -> ExpressionList
parse = concatMap parseLine . lines

parseLine :: Text -> ExpressionList
parseLine = readExpressionList . filter01 . Text.takeWhile (/= '#')

filter01 :: Text -> Text
filter01 = Text.filter is01

is01 :: Char -> Bool
is01 c = c == '0' || c == '1'
