module HelVM.HelMA.Automata.Zot.Parser (
  parse,
) where

import           HelVM.HelMA.Automata.Zot.Expression

import qualified Data.Text.Lazy                      as LText

parse ∷ LText → ExpressionList
parse = concatMap parseLine . LText.lines

parseLine ∷ LText → ExpressionList
parseLine = readExpressionList . filter01 . LText.takeWhile (/= '#')

filter01 ∷ LText → LText
filter01 = LText.filter is01

is01 ∷ Char → Bool
is01 c = c == '0' || c == '1'
