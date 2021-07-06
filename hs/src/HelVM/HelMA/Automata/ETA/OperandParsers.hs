module HelVM.HelMA.Automata.ETA.OperandParsers where

import HelVM.HelMA.Automata.ETA.Symbol
import HelVM.HelMA.Automata.ETA.Token

import HelVM.Common.Containers.Lookup
import HelVM.Common.Safe
import HelVM.Common.Digit.ToDigit

data InstructionUnit = IU !TokenList !InstructionCounter
  deriving stock (Show)

type OperandIUParser a = InstructionUnit -> Safe (a , InstructionUnit)

parseNumber :: (Integral a) => OperandIUParser a
parseNumber iu = parseNumber' [] =<< nextIU iu

parseNumber' :: Integral a => TokenList -> (Maybe Token, InstructionUnit) -> Safe (a, InstructionUnit)
parseNumber' acc (Nothing , iu) = ( , iu) <$> makeIntegral 7 acc
parseNumber' acc (Just E  , iu) = ( , iu) <$> makeIntegral 7 acc
parseNumber' acc (Just R  , iu) = parseNumber'    acc  =<< nextIU iu
parseNumber' acc (Just t  , iu) = parseNumber' (t:acc) =<< nextIU iu

nextIU :: OperandIUParser (Maybe Token)
nextIU iu@(IU il ic)
  | ic < length il = wrap <$> indexSafe il ic
  | otherwise      = pure (Nothing , iu)
  where wrap i = (Just i, IU il (ic+1))
