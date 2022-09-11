module HelVM.HelMA.Automaton.ReadPExtra where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Text.ParserCombinators.ReadP      hiding (many)

runParser :: MonadSafe m => ReadP a -> Source -> m a
runParser parser source = fst . last <$> nonEmptyRunParser parser source

nonEmptyRunParser :: MonadSafe m => ReadP a -> Source -> m $ NonEmpty (a , String)
nonEmptyRunParser parser source = nonEmptyFromList ("Cannot parse source\n" <> source) $ listRunParser parser source

listRunParser :: ReadP a -> Source -> [(a , String)]
listRunParser parser = readP_to_S parser . toString

oneOf :: String -> ReadP Char
oneOf cs = satisfy (`elem` cs)

manyNonEmpty :: Alternative f => f a -> f $ NonEmpty a
manyNonEmpty p = liftA2 (:|) p (many p)
