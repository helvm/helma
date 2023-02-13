module HelVM.HelMA.Automata.FALSE.Parser (
  parseSafe,
  parse,
  charToSimpleInstruction,
) where

import           HelVM.HelMA.Automata.FALSE.Expression

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.SInstruction
import           HelVM.HelMA.Automaton.ReadPExtra

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra                               hiding (runParser)
import           HelVM.HelIO.ReadText

import           Data.Char

import           Text.ParserCombinators.ReadP                    hiding (many)

parseSafe :: Source -> Safe ExpressionList
parseSafe = parse

parse :: MonadSafe m => Source -> m ExpressionList
parse = runParser vlParser

vlParser :: ReadP ExpressionList
vlParser = many (skipSpaces *> valueParser) <* skipSpaces

valueParser :: ReadP Expression
valueParser = lambdaParser <|> commentParser <|> writeStringParser <|> constParser <|> refParser <|> simpleParser

lambdaParser :: ReadP Expression
lambdaParser = Lambda <$> (char '[' *> vlParser <* char ']')

commentParser :: ReadP Expression
commentParser = Comment <$> (char '{' *> many (notChar '}') <* char '}')

writeStringParser :: ReadP Expression
writeStringParser = Str <$> stringParser

constParser :: ReadP Expression
constParser = Inst . consI . fromIntegral <$> naturalParser

refParser :: ReadP Expression
refParser = refFromChar <$> letterAscii

simpleParser :: ReadP Expression
simpleParser = fromJustWithText "imposible" . charToSimpleInstruction <$> oneOf simpleInstructionChars

simpleInstructionChars :: String
simpleInstructionChars = "$%\\@`+-*/_&|~<=!?#:;^,.ß"

charToSimpleInstruction :: Char -> Maybe Expression
charToSimpleInstruction '$'  = inst dupI
charToSimpleInstruction '%'  = inst discardI
charToSimpleInstruction '\\' = inst swapI
charToSimpleInstruction '@'  = inst rotI
charToSimpleInstruction '`'  = inst copyTI

charToSimpleInstruction '+'  = inst addI
charToSimpleInstruction '-'  = inst subI
charToSimpleInstruction '*'  = inst mulI
charToSimpleInstruction '/'  = inst divI
charToSimpleInstruction '_'  = inst negI

charToSimpleInstruction '&'  = inst $ binary BAnd
charToSimpleInstruction '|'  = inst $ binary BOr
charToSimpleInstruction '~'  = inst $ unary BNot

charToSimpleInstruction '<'  = inst $ binary LGT
charToSimpleInstruction '='  = inst $ binary LEQ

charToSimpleInstruction '!'  = pure Exec
charToSimpleInstruction '?'  = pure Cond
charToSimpleInstruction '#'  = pure While

charToSimpleInstruction ':'  = pure Store
charToSimpleInstruction ';'  = pure Fetch

charToSimpleInstruction '^'  = inst $ sio InputChar
charToSimpleInstruction ','  = inst $ sio OutputChar
charToSimpleInstruction '.'  = inst $ sio OutputDec
charToSimpleInstruction 'ß'  = pure Flush

charToSimpleInstruction  _   = Nothing

inst :: Instruction -> Maybe Expression
inst = pure . Inst

-- | Extra

refFromChar :: Char -> Expression
refFromChar c = Ref $ fromIntegral $ ord (toLower c) - ord 'a'

naturalParser :: ReadP Natural
naturalParser = naturalLiteralParser <|> ordCharLiteralParser

naturalLiteralParser :: ReadP Natural
naturalLiteralParser = readUnsafe <$> many1 digit

ordCharLiteralParser :: Integral a => ReadP a
ordCharLiteralParser = fromIntegral . ord <$> (skipSpacesAndChar '\'' *> anyChar)

stringParser :: ReadP String
stringParser = skipSpacesAndChar '"' *> many (notChar '"') <* char '"'

skipSpacesAndChar :: Char -> ReadP Char
skipSpacesAndChar c = skipSpaces *> char c
