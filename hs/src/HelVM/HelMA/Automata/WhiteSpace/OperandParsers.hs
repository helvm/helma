module HelVM.HelMA.Automata.WhiteSpace.OperandParsers where

import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.Common.Collections.SList
import           HelVM.Common.Digit.ToDigit
import           HelVM.Common.Safe

type OperandParser a = TokenList -> Safe (a , TokenList)

parseInt :: OperandParser Int
parseInt tl = parseInt' <$> parseInteger tl where
  parseInt' (integer , tl') = (fromIntegral integer , tl')

parseInteger :: OperandParser Integer
parseInteger []       = liftError "EOL"
parseInteger (S : tl) = parseUtil makeIntegral2' tl
parseInteger (T : tl) = negationIntegral <$> parseUtil makeIntegral2' tl
parseInteger (N : tl) = pure (0 , tl)

negationIntegral :: (Integer , TokenList) -> (Integer , TokenList)
negationIntegral (i , l) = (-i , l)

parseNatural :: OperandParser Natural
parseNatural = parseUtil makeIntegral2'

parseUtil :: (TokenList -> Safe a) -> OperandParser a
parseUtil maker = go ([] :: TokenList) where
  go acc []     = liftError $ show acc
  go acc (N:tl) = moveSafe (maker acc , tl)
  go acc (t:tl) = go (t : acc) tl

parseDigitString :: OperandParser SString
parseDigitString tl = moveSafe =<< parseString' makeDigitString' tl

parseAsciiString :: OperandParser SString
parseAsciiString tl = moveSafe =<< parseString' makeAsciiString28' tl

moveSafe :: (Safe a , TokenList) -> Safe (a , TokenList)
moveSafe (a , tl) = appendErrorTuple ("TokenList" , show tl) $ ( , tl) <$> a

parseString' :: (TokenList -> a) -> OperandParser a
parseString' maker tl = parseString'' <$> splitByN tl where
  parseString'' (acc , tl') = (maker acc , tl')

splitByN :: OperandParser TokenList
splitByN []       = liftError "Empty list"
splitByN (N : tl) = pure ([]    , tl)
splitByN (t : tl) = splitByN' <$> splitByN tl where
  splitByN' (acc , tl') = (t:acc , tl')

makeDigitString' :: (MonadSafeError m , ToDigit a) => [a] -> m SString
makeDigitString' = makeDigitString . fromList

makeAsciiString28' :: (MonadSafeError m , ToDigit a) => [a] -> m SString
makeAsciiString28' = makeAsciiString28 . fromList

makeIntegral2' :: (MonadSafeError m , ToDigit a, Integral b) => [a] -> m b
makeIntegral2' = makeIntegral2 . fromList

