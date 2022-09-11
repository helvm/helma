module HelVM.HelMA.Automata.LazyK.Parsers.MixedParser (
  parse,
) where

import           HelVM.HelMA.Automata.LazyK.Lambda
import           HelVM.HelMA.Automata.LazyK.Lexer

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

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

-- | ParserUtils
runParser :: MonadSafe m => ReadP Lambda -> Source -> m Lambda
runParser parser source = fst . last <$> nonEmptyRunParser parser source

nonEmptyRunParser :: MonadSafe m => ReadP Lambda -> Source -> m $ NonEmpty (Lambda , String)
nonEmptyRunParser parser source = nonEmptyFromList ("Cannot parse source\n" <> source) $ listRunParser parser source

listRunParser :: ReadP Lambda -> Source -> [(Lambda , String)]
listRunParser parser = readP_to_S parser . toString

oneOf :: String -> ReadP Char
oneOf cs = satisfy (`elem` cs)

manyNonEmpty :: Alternative f => f a -> f $ NonEmpty a
manyNonEmpty p = liftA2 (:|) p (many p)
