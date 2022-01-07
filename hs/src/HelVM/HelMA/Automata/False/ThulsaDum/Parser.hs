module HelVM.HelMA.Automata.False.ThulsaDum.Parser ( parseFalse
                    , Value (..)
                    , FalseParser
                    ) where

import           HelVM.HelMA.Automata.False.Value

import           HelVM.HelMA.Automaton.Operator.BinaryOperator
import           HelVM.HelMA.Automaton.Operator.HighControlOperator
import           HelVM.HelMA.Automaton.Operator.IOOperator
import           HelVM.HelMA.Automaton.Operator.StackOperator
import           HelVM.HelMA.Automaton.Operator.UnaryOperator

import           HelVM.HelIO.ReadText

import           Text.Parsec                                        hiding (many, (<|>))
import           Text.Parsec.String

parseFalse :: Read i => FalseParser i
-- FIXME it should be possible to write an empty program
parseFalse = many1 parseFalseValue

parseFalseValue :: Read i => Parser (Value i)
parseFalseValue = parseFnord *> parseValue <* parseFnord

parseValue :: Read i => Parser (Value i)
parseValue =
  parseAdd
  <|> parseSub
  <|> parseMul
  <|> parseDiv
  <|> parseNeg
  <|> parseEq
  <|> parseGt
  <|> parseAnd
  <|> parseOr
  <|> parseNot
  <|> parseApply
  <|> parseDup
  <|> parseDrop
  <|> parseSwap
  <|> parseRot
  <|> parsePick
  <|> parseWhen
  <|> parseWhile
  <|> parsePrintInt
  <|> parseFlush
  <|> parsePutch
  <|> parseGetch
  <|> try parsePutVar
  <|> try parseGetVar
  <|> parseIntLit
  <|> parseCharLit
--  <|> parseVarRef
  <|> parseLambda
  <|> parsePrintString


parseAdd, parseSub, parseMul, parseDiv, parseNeg, parseEq, parseGt
  , parseAnd, parseOr, parseNot, parseApply, parseDup, parseDrop, parseSwap
  , parseRot, parsePick, parseWhen, parseWhile, parsePrintInt, parseFlush
  , parsePutch, parseGetch, parsePutVar, parseGetVar :: Parser (Value i)

parseAdd = genSimpleParser '+' $ Binary Add
parseSub = genSimpleParser '-' $ Binary Sub
parseMul = genSimpleParser '*' $ Binary Mul
parseDiv = genSimpleParser '/' $ Binary Div
parseNeg = genSimpleParser '_' $ Unary  Neg
parseEq  = genSimpleParser '=' $ Binary BEQ
parseGt  = genSimpleParser '>' $ Binary BGT
parseAnd = genSimpleParser '&' $ Binary BAnd
parseOr  = genSimpleParser '|' $ Binary BOr
parseNot = genSimpleParser '~' $ Unary  BNot
parseApply = genSimpleParser '!' $ High Apply
parseDup   = genSimpleParser '$' $ Stack Dup
parseDrop  = genSimpleParser '%' $ Stack Discard
parseSwap  = genSimpleParser '\\' $ Stack Swap
parseRot   = genSimpleParser '@' $ Stack Rot
parsePick  = genSimpleParser '`' (Stack DCopy)
             <|> genSimpleParser 'ø' (Stack DCopy) -- NOTE The standard defines pick as ø
parseWhen  = genSimpleParser '?' $ High When
parseWhile = genSimpleParser '#' $ High While
parsePrintInt = genSimpleParser '.' $ IOStack OutputDec
parsePutch    = genSimpleParser ',' $ IOStack OutputChar
parseGetch    = genSimpleParser '^' $ IOStack InputChar
parseFlush    = genSimpleParser 'ß' Flush   -- TODO reassign

parsePutVar   = do
  var <- lower
  genSimpleParser ':' $ PutVar var
parseGetVar   = do
  var <- lower
  genSimpleParser ';' $ GetVar var

genSimpleParser :: Char -> Value i -> Parser (Value i)
genSimpleParser ch tok = char ch $> tok


parseIntLit :: Read i => Parser (Value i)
parseIntLit = IntLit . readUnsafe <$> many1 digit

parseCharLit :: Parser (Value i)
parseCharLit = CharLit <$> (char '\'' *> anyChar)

-- parseVarRef :: Parser (Value i)
-- parseVarRef = lower >>= (pure . VarRef)

parseLambda :: Read i => Parser (Value i)
parseLambda = Lambda <$> (char '[' *> parseFalse <* char ']')

parsePrintString :: Parser (Value i)
parsePrintString = Print <$> (char '"' *> many (noneOf "\"") <* char '"')

parseComment :: Parser ()
parseComment = char '{' *> many (noneOf "}") *> char '}' *> pass

parseSpace :: Parser ()
parseSpace  = space *> pass

parseFnord :: Parser ()
parseFnord = skipMany $ parseComment <|> parseSpace

-- | Types

type FalseParser i = Parser [Value i]
