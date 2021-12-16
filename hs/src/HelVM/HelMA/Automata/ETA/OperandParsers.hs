module HelVM.HelMA.Automata.ETA.OperandParsers where

import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.Common.Containers.LLIndexSafe
import           HelVM.Common.Control.Safe
import           HelVM.Common.Digit.ToDigit

import qualified Data.Vector                         as Vector

data InstructionUnit = IU !TokenVector !InstructionCounter
  deriving stock (Show)

type OperandIUParser a = InstructionUnit -> Safe (a , InstructionUnit)

parseNumber :: (Integral a) => OperandIUParser a
parseNumber iu = go [] =<< nextIU iu where
  go :: Integral a => TokenList -> (Maybe Token, InstructionUnit) -> Safe (a, InstructionUnit)
  go acc (Nothing , iu') = ( , iu') <$> makeIntegral7FromList acc
  go acc (Just E  , iu') = ( , iu') <$> makeIntegral7FromList acc
  go acc (Just R  , iu') = go      acc  =<< nextIU iu'
  go acc (Just t  , iu') = go (t : acc) =<< nextIU iu'

nextIU :: OperandIUParser (Maybe Token)
nextIU iu@(IU il ic)
  | ic < Vector.length il = wrap <$> indexSafe il ic
  | otherwise             = pure (Nothing , iu)
  where wrap i = (Just i, IU il (ic+1))
