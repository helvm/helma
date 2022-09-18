module HelVM.HelMA.Automaton.ReadPExtra where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Data.Char

import           Text.ParserCombinators.ReadP      hiding (many)

runParser :: MonadSafe m => ReadP a -> Source -> m a
runParser parser source = fst . last <$> nonEmptyRunParser parser source

nonEmptyRunParser :: MonadSafe m => ReadP a -> Source -> m $ NonEmpty (a , String)
nonEmptyRunParser parser source = nonEmptyFromList ("Cannot parse source\n" <> source) $ listRunParser parser source

listRunParser :: ReadP a -> Source -> [(a , String)]
listRunParser parser = readP_to_S parser . toString

-- | Parsers

oneOf :: String -> ReadP Char
oneOf cs = satisfy (`elem` cs)

notChar :: Char -> ReadP Char
notChar c = satisfy (/= c)

anyChar :: ReadP Char
anyChar = satisfy $ const True

digit :: ReadP Char
digit = satisfy isDigit

letterAscii :: ReadP Char
letterAscii = satisfy isAlphaAscii

-- | Extra

isAlphaAscii :: Char -> Bool
isAlphaAscii c = isAsciiLower c || isAsciiUpper c

manyNonEmpty :: Alternative f => f a -> f $ NonEmpty a
manyNonEmpty p = liftA2 (:|) p (many p)
