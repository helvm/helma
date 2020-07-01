module HelVM.HelCam.Machines.WhiteSpace.OperandParsers where

import HelVM.HelCam.Machines.WhiteSpace.Token

import HelVM.HelCam.Common.Util

import Data.Char
import Numeric.Natural

type OperandParser a = TokenList -> (a, TokenList)

parseInt :: OperandParser Int
parseInt tokens = (fromIntegral integer, tokens') where (integer, tokens') = parseInteger tokens

parseInteger :: OperandParser Integer
parseInteger []         = error "EOL"
parseInteger (S:tokens) = parseUtil makeIntegral tokens
parseInteger (T:tokens) = negationIntegral $ parseUtil makeIntegral tokens
parseInteger (N:tokens) = (0,tokens)

negationIntegral :: (Integer, TokenList) -> (Integer, TokenList)
negationIntegral (i,l) = (-i,l)

parseNatural :: OperandParser Natural
parseNatural = parseUtil makeIntegral

parseUtil :: (TokenList -> a) -> OperandParser a
parseUtil maker = parseUtil' ([]::TokenList) where
  parseUtil' acc []         = error $ show acc
  parseUtil' acc (N:tokens) = (maker acc, tokens)
  parseUtil' acc (t:tokens) = parseUtil' (t:acc) tokens

parseBitString :: OperandParser String
parseBitString = parseString' makeBitString

parseAsciiString :: OperandParser String
parseAsciiString = parseString' makeAsciiString

parseString' :: (TokenList -> a) -> OperandParser a
parseString' maker tokens = (maker acc, tokens') where (acc, tokens') = splitByN tokens

splitByN :: OperandParser TokenList
splitByN []         = error $ show "Empty list"
splitByN (N:tokens) = ([], tokens)
splitByN (t:tokens) = (t:acc, tokens') where (acc, tokens') = splitByN tokens

----

makeIntegral :: (Integral a) => TokenList -> a
makeIntegral = foldr (shiftAndAdd . toBit) 0

shiftAndAdd :: (Integral a) => a -> a -> a
shiftAndAdd bit acc = acc * 2 + bit

makeBitString :: TokenList -> String
makeBitString = map toBitChar

makeAsciiString :: TokenList -> String
makeAsciiString tokens = map makeChar $ chunksOf 8 tokens

makeChar :: TokenList -> Char
makeChar = chr . makeIntegral . reverse
