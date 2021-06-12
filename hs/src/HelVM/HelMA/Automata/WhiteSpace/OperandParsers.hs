module HelVM.HelMA.Automata.WhiteSpace.OperandParsers where

import HelVM.HelMA.Automata.WhiteSpace.Token

import HelVM.Common.Safe
import HelVM.Common.Digit.ToDigit

type OperandParser a = TokenList -> Safe (a , TokenList)

parseInt :: OperandParser Int
parseInt tl = parseInt' <$> parseInteger tl where
  parseInt' (integer , tl') = (fromIntegral integer , tl')

parseInteger :: OperandParser Integer
parseInteger []     = safeError "EOL"
parseInteger (S:tl) = parseUtil (makeIntegral 2) tl
parseInteger (T:tl) = negationIntegral <$> parseUtil (makeIntegral 2) tl
parseInteger (N:tl) = safe (0 , tl)

negationIntegral :: (Integer , TokenList) -> (Integer , TokenList)
negationIntegral (i , l) = (-i , l)

parseNatural :: OperandParser Natural
parseNatural = parseUtil (makeIntegral 2)

parseUtil :: (TokenList -> Safe a) -> OperandParser a
parseUtil maker = parseUtil' ([]::TokenList) where
  parseUtil' acc []     = safeError $ show acc
  parseUtil' acc (N:tl) = moveSafe (maker acc , tl)
  parseUtil' acc (t:tl) = parseUtil' (t:acc) tl

parseDigitString :: OperandParser String
parseDigitString tl = moveSafe =<< parseString' makeDigitString tl

parseAsciiString :: OperandParser String
parseAsciiString tl = moveSafe =<< parseString' (makeAsciiString 2 8) tl

moveSafe :: (Safe a , TokenList) -> Safe (a , TokenList)
moveSafe (a , tl) = appendErrorTuple ("TokenList" , show tl) $ ( , tl) <$> a

parseString' :: (TokenList -> a) -> OperandParser a
parseString' maker tl = parseString'' <$> splitByN tl where
  parseString'' (acc , tl') = (maker acc , tl')

splitByN :: OperandParser TokenList
splitByN []         = safeError "Empty list"
splitByN (N:tl) = safe ([]    , tl)
splitByN (t:tl) = splitByN' <$> splitByN tl where
  splitByN' (acc , tl') = (t:acc , tl')
