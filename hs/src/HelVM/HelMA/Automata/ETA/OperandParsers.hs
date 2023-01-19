module HelVM.HelMA.Automata.ETA.OperandParsers where

import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Digit.ToDigit

import           Control.Monad.Extra

import qualified Data.Vector                        as Vector

parseNumberFromTLL :: (MonadSafe m , Integral a) => (TokenList, [TokenList]) -> m (a , (TokenList, [TokenList]))
parseNumberFromTLL a = loop act ([] , a) where
  act (acc , (E  : tl , tll))      = Right $ ( , (tl , tll)) <$> makeIntegral7FromList acc
  act (acc , (R  : tl , tll))      = Left (    acc , (tl , tll))
  act (acc , (t  : tl , tll))      = Left (t : acc , (tl , tll))
  act (acc ,      ([] , tl : tll)) = Left (    acc , (tl , tll))
--  act (acc ,      ([] , []))       = Right $ pure (0, ([] , []))
  act (acc ,      ([] , []))       = Right $ ( , ([] , [])) <$> makeIntegral7FromList acc

parseNumberFromTL :: (MonadSafe m , Integral a) => OperandParser m a
parseNumberFromTL a = loop act ([] , a) where
  act (acc , E  : tl) = Right $ ( , tl) <$> makeIntegral7FromList acc
  act (acc , R  : tl) = Left (    acc , tl)
  act (acc , t  : tl) = Left (t : acc , tl)
  act (acc ,      []) = Right (liftError $ show acc)

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

updatePC :: InstructionUnit -> InstructionCounter -> InstructionUnit
updatePC iu a = iu { programCounter = a }

-- | Types
type OperandParser m a = TokenList -> m (a , TokenList)

data InstructionUnit = IU
  { program        :: !TokenVector
  , programCounter :: !InstructionCounter
  } deriving stock (Eq , Read , Show)

type OperandIUParser m a = InstructionUnit -> m (a , InstructionUnit)
