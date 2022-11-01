module HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Parser (
  parseAsVectorSafe,
  parseAsVector,
) where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Instruction    as Tree

import           HelVM.HelMA.Automata.BrainFuck.Common.SimpleInstruction

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.ReadPExtra

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra                                       hiding (runParser)

import qualified Data.Text                                               as Text
import qualified Data.Vector                                             as Vector

import           Text.ParserCombinators.ReadP                            hiding (many)

parseAsVectorSafe :: Source -> Safe TreeInstructionVector
parseAsVectorSafe = parseAsVector

parseAsVector :: MonadSafe m => Source -> m TreeInstructionVector
parseAsVector = runParser treeInstructionsParser . filterComments

treeInstructionsParser :: ReadP TreeInstructionVector
treeInstructionsParser = Vector.fromList <$> many treeInstructionParser

treeInstructionParser :: ReadP TreeInstruction
treeInstructionParser = simpleParser <|> whileParser

whileParser :: ReadP TreeInstruction
whileParser = Tree.While <$> (char '[' *> treeInstructionsParser <* char ']')

simpleParser :: ReadP TreeInstruction
simpleParser =  Simple . fromJustWithText "imposible" . charToSimpleInstruction <$> oneOf simpleInstructionChars

filterComments :: Source -> Source
filterComments = Text.filter isNotComment

isNotComment :: Char -> Bool
isNotComment c = c `elem` allInstructionChars

allInstructionChars :: String
allInstructionChars = "[]" <> simpleInstructionChars

simpleInstructionChars :: String
simpleInstructionChars = show =<< simpleInstructions
