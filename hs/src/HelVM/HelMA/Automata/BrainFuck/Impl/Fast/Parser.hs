module HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Parser (
  parseWithOptimizeSafe,
  parseAsListSafe,
  parseWithOptimize,
  parseAsList,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Instruction
import           HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Optimizer

import qualified HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction as Simple

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.ReadPExtra

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.Tools

import qualified Data.Text                                               as Text

import           Text.ParserCombinators.ReadP                            hiding (many)

parseWithOptimizeSafe :: Source -> Safe FastInstructionList
parseWithOptimizeSafe = parseWithOptimize

parseAsListSafe :: Source -> Safe FastInstructionList
parseAsListSafe = parseAsList

parseWithOptimize :: MonadSafe m => Source -> m FastInstructionList
parseWithOptimize = optimize <.> parseAsList

parseAsList :: MonadSafe m => Source -> m FastInstructionList
parseAsList = runParser parameterizedInstructionsParser . filterComments

parameterizedInstructionsParser :: ReadP FastInstructionList
parameterizedInstructionsParser = many1 parameterizedInstructionParser

parameterizedInstructionParser :: ReadP FastInstruction
parameterizedInstructionParser =
      moveRParser <|> moveLParser
  <|> incParser   <|> decParser
  <|> outParser   <|> inParser
  <|> whileParser

moveRParser :: ReadP FastInstruction
moveRParser = Move 1 <$ char '>'

moveLParser :: ReadP FastInstruction
moveLParser = Move negate1 <$ char '<'

incParser :: ReadP FastInstruction
incParser = Inc 1 <$ char '+'

decParser :: ReadP FastInstruction
decParser = Inc negate1 <$ char '-'

outParser :: ReadP FastInstruction
outParser = Output <$ char '.'

inParser :: ReadP FastInstruction
inParser = Input <$ char ','

whileParser :: ReadP FastInstruction
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

negate1 :: Integer
negate1 = negate 1
