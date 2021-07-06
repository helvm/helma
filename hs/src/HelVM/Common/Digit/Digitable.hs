module HelVM.Common.Digit.Digitable where

import HelVM.Common.Digit.ToDigit

import HelVM.Common.Safe

class ToDigit t => Digitable t where
  fromDigit :: (MonadSafeError m , Show a , Integral a) => a -> m t
