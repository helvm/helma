module HelVM.HelMA.Automaton.Dynamic.DynamicIntegral where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelMA.Automaton.BIntegral

class BIntegral i => DynamicIntegral i a | a -> i where
  -- | Constructors
  toDynamicIntegral         :: i -> a
  -- | Destructors
  fromDynamicIntegral       :: MonadSafe m => a -> m i
  fromDynamicIntegralUnsafe :: a -> i
  fromDynamicIntegralUnsafe a = unsafe $ fromDynamicIntegral a
  aaa :: (i -> i) -> a -> a
  aaa f = toDynamicIntegral . f . fromDynamicIntegralUnsafe
