module HelVM.HelMA.Automaton.Dynamic.DynamicVar where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelMA.Automaton.BIntegral

class DynamicVar a where
  -- | Constructors
  toDynamicVar         :: BIntegral i => i -> a
  -- | Destructors
  fromDynamicVar       :: (MonadSafe m , BIntegral i) => a -> m i
  fromDynamicVarUnsafe :: BIntegral i => a -> i
  fromDynamicVarUnsafe a = unsafe $ fromDynamicVar a
