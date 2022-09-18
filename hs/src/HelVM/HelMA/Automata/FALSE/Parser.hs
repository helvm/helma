module HelVM.HelMA.Automata.FALSE.Parser (
  parseSafe,
  parse,
) where

import           HelVM.HelMA.Automata.FALSE.Value

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.ALInstruction
import           HelVM.HelMA.Automaton.Instruction.IOInstruction
import           HelVM.HelMA.Automaton.ReadPExtra

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ReadText

import           Data.Char

import           Text.ParserCombinators.ReadP                    hiding (many)

parseSafe :: Source -> Safe ValueList
parseSafe = parse

parse :: MonadSafe m => Source -> m ValueList
parse = runParser vlParser

vlParser :: ReadP ValueList
vlParser = many (skipSpaces *> valueParser) <* skipSpaces

valueParser :: ReadP Value
valueParser =
      constParser
  <|> dupParser <|> dropParser <|> swapParser <|> rotParser <|> pickParser
  <|> addParser <|> subParser <|> mulParser <|> divParser <|> negParser
  <|> andParser <|> orParser <|> notParser
  <|> gtParser <|> eqParser
  <|> lambdaParser <|> execParser <|> condParser <|> whileParser
  <|> refParser <|> stParser <|> ldParser
  <|> readCharParser <|> writeCharParser <|> writeStringParser <|> writeNumParser <|> flushParser
  <|> commentParser

constParser :: ReadP Value
constParser = Inst . IAL . Cons . fromIntegral <$> naturalParser

dupParser , dropParser , swapParser , rotParser , pickParser :: ReadP Value
dupParser  = Inst (IAL Dup    ) <$ char '$'
dropParser = Inst (IAL Discard) <$ char '%'
swapParser = Inst (IAL Swap   ) <$ char '\\'
rotParser  = Inst (IAL Rot    ) <$ char '@'
pickParser = Inst (IAL Pick   ) <$ char '`'

addParser , subParser , mulParser , divParser , negParser :: ReadP Value
addParser = Inst (IAL (Binary Add)) <$ char '+'
subParser = Inst (IAL (Binary Sub)) <$ char '-'
mulParser = Inst (IAL (Binary Mul)) <$ char '*'
divParser = Inst (IAL (Binary Div)) <$ char '/'
negParser = Inst (IAL (Unary  Neg)) <$ char '_'

andParser , orParser , notParser :: ReadP Value
andParser = Inst (IAL (Binary BAnd)) <$ char '&'
orParser  = Inst (IAL (Binary BOr )) <$ char '|'
notParser = Inst (IAL (Unary  BNot)) <$ char '~'

gtParser , eqParser :: ReadP Value
gtParser = Inst (IAL (Binary LGT)) <$ char '<'
eqParser = Inst (IAL (Binary LEQ)) <$ char '='

lambdaParser , execParser , condParser , whileParser  :: ReadP Value
lambdaParser = Lambda <$> (char '[' *> vlParser <* char ']')
execParser   = Exec <$ char '!'
condParser   = Cond <$ char '?'
whileParser  = While <$ char '#'

refParser , stParser , ldParser :: ReadP Value
refParser = refFromChar <$> letterAscii
stParser  = Store <$ char ':'
ldParser  = Fetch <$ char ';'

readCharParser , writeCharParser , writeNumParser , writeStringParser , flushParser:: ReadP Value
readCharParser    = Inst (IAL (SIO InputChar )) <$ char '^'
writeCharParser   = Inst (IAL (SIO OutputChar)) <$ char ','
writeStringParser = Str <$> stringParser
writeNumParser    = Inst (IAL (SIO OutputDec )) <$ char '.'
flushParser       = Flush <$ char 'ß'

commentParser :: ReadP Value
commentParser = Comment <$> (char '{' *> many (notChar '}') <* char '}')

-- | Extra

refFromChar :: Char -> Value
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
