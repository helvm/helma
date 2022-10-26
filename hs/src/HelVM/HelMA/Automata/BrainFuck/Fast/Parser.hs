module HelVM.HelMA.Automata.BrainFuck.Fast.Parser (
  parseAsVector,
) where

import           HelVM.HelMA.Automata.BrainFuck.Fast.Instruction

import qualified HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction as Simple

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.ReadPExtra

import           HelVM.HelIO.Control.Safe

--import qualified Data.ListLike                                                       as LL
import qualified Data.Text                                               as Text
import qualified Data.Vector                                             as Vector

import           Text.ParserCombinators.ReadP                            hiding (many)

parseAsVector :: MonadSafe m => Source -> m SomeInstructionVector
parseAsVector = runParser parameterizedInstructionsParser . filterComments

parameterizedInstructionsParser :: ReadP SomeInstructionVector
parameterizedInstructionsParser = Vector.fromList <$> many1 parameterizedInstructionParser

parameterizedInstructionParser :: ReadP SomeInstruction
parameterizedInstructionParser =
      moveRParser <|> moveLParser
  <|> incParser   <|> decParser
  <|> outParser   <|> inParser
  <|> whileParser

--moveRParser :: ReadP SomeInstruction
--moveRParser = Move . LL.length <$> many1 (char '>')
--
--moveLParser :: ReadP SomeInstruction
--moveLParser = Move . negate . LL.length <$> many1 (char '<')
--
--incParser :: ReadP SomeInstruction
--incParser = Inc . LL.length <$> many1 (char '+')
--
--decParser :: ReadP SomeInstruction
--decParser = Inc . negate . LL.length <$> many1 (char '-')

moveRParser :: ReadP SomeInstruction
moveRParser = Move 1 <$ char '>'

moveLParser :: ReadP SomeInstruction
moveLParser = Move negate1 <$ char '<'

incParser :: ReadP SomeInstruction
incParser = Inc 1 <$ char '+'

decParser :: ReadP SomeInstruction
decParser = Inc negate1 <$ char '-'

outParser :: ReadP SomeInstruction
outParser = Output <$ char '.'

inParser :: ReadP SomeInstruction
inParser = Input <$ char ','

whileParser :: ReadP SomeInstruction
whileParser = While <$> (char '[' *> parameterizedInstructionsParser <* char ']')

filterComments :: Source -> Source
filterComments = Text.filter isNotComment

isNotComment :: Char -> Bool
isNotComment c = c `elem` allInstructionChars

allInstructionChars :: String
allInstructionChars = "[]" <> simpleInstructionChars

simpleInstructionChars :: String
simpleInstructionChars = show =<< Simple.simpleInstructions

--

negate1 :: Int
negate1 = negate 1
