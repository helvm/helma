module HelVM.HelMA.Automata.LazyK.Parser (
  parse,
) where

import           HelVM.HelMA.Automata.LazyK.Lambda
import           HelVM.HelMA.Automata.LazyK.Lexer

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.ReadPExtra

import           HelVM.HelIO.Control.Safe

import           Text.ParserCombinators.ReadP      hiding (many)

parse :: MonadSafe m => Source -> m Lambda
parse = parseCode . filterComments

parseCode :: MonadSafe m => Source -> m Lambda
parseCode = runParser appParser

appParser :: ReadP Lambda
appParser = foldlLambda <$> manyNonEmpty lambdaParser

lambdaParser :: ReadP Lambda
lambdaParser =
       S <$ oneOf "sS"
  <|>  K <$ oneOf "kK"
  <|>  I <$ oneOf "iI"
  <|>  App <$ char '`' <*> lambdaParser <*> lambdaParser
  <|>  char '(' *> appParser <* char ')'
