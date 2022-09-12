module HelVM.HelMA.Automata.WhiteSpace.OperandParsers where

import           HelVM.HelMA.Automata.WhiteSpace.Symbol
import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.HelMA.Automaton.Instruction.ControlInstruction

import           HelVM.HelIO.Collections.SList
import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Digit.ToDigit

type OperandParser m a = TokenList -> m (a , TokenList)

----

parseIndex :: MonadSafe m => OperandParser m Index
parseIndex = parseInt

parseSymbol :: MonadSafe m => OperandParser m Symbol
parseSymbol = parseInteger

parseLabel :: MonadSafe m => Bool -> OperandParser m Label
parseLabel False = parseDigitString
parseLabel True  = parseAsciiString

----

parseInt :: MonadSafe m => OperandParser m Int
parseInt tl = parseInt' <$> parseInteger tl where
  parseInt' (integer , tl') = (fromIntegral integer , tl')

parseInteger :: MonadSafe m => OperandParser m Integer
parseInteger []       = liftError "EOL"
parseInteger (S : tl) = parseExtra makeIntegral2FromList tl
parseInteger (T : tl) = negationIntegral <$> parseExtra makeIntegral2FromList tl
parseInteger (N : tl) = pure (0 , tl)

negationIntegral :: (Integer , TokenList) -> (Integer , TokenList)
negationIntegral (i , l) = (-i , l)

parseNatural :: MonadSafe m => OperandParser m Natural
parseNatural = parseExtra makeIntegral2FromList

parseExtra :: MonadSafe m => (TokenList -> m a) -> OperandParser m a
parseExtra maker = go ([] :: TokenList) where
  go acc []     = liftError $ show acc
  go acc (N:tl) = moveSafe (maker acc , tl)
  go acc (t:tl) = go (t : acc) tl

parseDigitString :: MonadSafe m => OperandParser m SString
parseDigitString tl = moveSafe =<< parseString' makeDigitStringFromList tl

parseAsciiString :: MonadSafe m => OperandParser m SString
parseAsciiString tl = moveSafe =<< parseString' makeAsciiString28FromList tl

moveSafe :: MonadSafe m => (m a , TokenList) -> m (a , TokenList)
moveSafe (a , tl) = appendErrorTuple ("TokenList" , show tl) $ ( , tl) <$> a

parseString' :: MonadSafe m => (TokenList -> a) -> OperandParser m a
parseString' maker tl = parseString'' <$> splitByN tl where
  parseString'' (acc , tl') = (maker acc , tl')

splitByN :: MonadSafe m => OperandParser m TokenList
splitByN []       = liftError "Empty list"
splitByN (N : tl) = pure ([]    , tl)
splitByN (t : tl) = splitByN' <$> splitByN tl where
  splitByN' (acc , tl') = (t:acc , tl')
