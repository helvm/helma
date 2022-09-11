module HelVM.HelMA.Automata.Zot.Parser (
  parse,
) where

import           HelVM.HelMA.Automata.Zot.Expression

import qualified Data.Text.Lazy                      as LT

parse :: LT.Text -> ExpressionList
parse = concatMap parseLine . LT.lines

parseLine :: LT.Text -> ExpressionList
parseLine = readExpressionList . filter01 . LT.takeWhile (/= '#')

filter01 :: LT.Text -> LT.Text
filter01 = LT.filter is01

is01 :: Char -> Bool
is01 c = c == '0' || c == '1'
