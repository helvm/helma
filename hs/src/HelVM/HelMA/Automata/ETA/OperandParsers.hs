module HelVM.HelMA.Automata.ETA.OperandParsers where

import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Digit.ToDigit

import           Control.Monad.Extra

import qualified Data.Vector                        as Vector

parseNumberFromTL :: (MonadSafe m , Integral a) => OperandParser m a
parseNumberFromTL tl = loop act ([] , tl) where
  act (acc ,       []) = Right $ ( , [])  <$> makeIntegral7FromList acc
  act (acc , E  : tl') = Right $ ( , tl') <$> makeIntegral7FromList acc
  act (acc , R  : tl') = Left (    acc , tl')
  act (acc , t  : tl') = Left (t : acc , tl')

parseNumber :: (MonadSafe m , Integral a) => OperandIUParser m a
parseNumber iu = loopM act =<< (([] , ) <$> nextIU iu) where
  act (acc , (Nothing , iu')) = Right . ( , iu') <$> makeIntegral7FromList acc
  act (acc , (Just E  , iu')) = Right . ( , iu') <$> makeIntegral7FromList acc
  act (acc , (Just R  , iu')) = Left  . (    acc , ) <$> nextIU iu'
  act (acc , (Just t  , iu')) = Left  . (t : acc , ) <$> nextIU iu'

nextIU :: MonadSafe m => OperandIUParser m (Maybe Token)
nextIU iu@(IU il ic)
  | ic < Vector.length il = wrap <$> indexSafe il ic
  | otherwise             = pure (Nothing , iu)
  where wrap i = (Just i, IU il (ic+1))

-- | Types
type OperandParser m a = TokenList -> m (a , TokenList)

data InstructionUnit = IU !TokenVector !InstructionCounter
  deriving stock (Eq , Read , Show)

type OperandIUParser m a = InstructionUnit -> m (a , InstructionUnit)
