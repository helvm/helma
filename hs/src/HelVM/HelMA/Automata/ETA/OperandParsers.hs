module HelVM.HelMA.Automata.ETA.OperandParsers where

import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.Common.Containers.LLIndexSafe
import           HelVM.Common.Control.Safe
import           HelVM.Common.Digit.ToDigit

import qualified Data.Vector                         as Vector

parseNumber :: (MonadSafe m , Integral a) => OperandIUParser m a
parseNumber iu = go [] =<< nextIU iu where
  go :: (MonadSafe m , Integral a) => TokenList -> (Maybe Token, InstructionUnit) -> m (a, InstructionUnit)
  go acc (Nothing , iu') = ( , iu') <$> makeIntegral7FromList acc
  go acc (Just E  , iu') = ( , iu') <$> makeIntegral7FromList acc
  go acc (Just R  , iu') = go      acc  =<< nextIU iu'
  go acc (Just t  , iu') = go (t : acc) =<< nextIU iu'

nextIU :: MonadSafe m => OperandIUParser m (Maybe Token)
nextIU iu@(IU il ic)
  | ic < Vector.length il = wrap <$> indexSafe il ic
  | otherwise             = pure (Nothing , iu)
  where wrap i = (Just i, IU il (ic+1))

-- | Types
data InstructionUnit = IU !TokenVector !InstructionCounter
  deriving stock (Eq , Read , Show)

type OperandIUParser m a = InstructionUnit -> m (a , InstructionUnit)
