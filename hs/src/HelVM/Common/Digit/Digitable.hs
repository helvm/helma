module HelVM.Common.Digit.Digitable where

import HelVM.Common.Digit.ToDigit
  
import HelVM.Common.Safe

class ToDigit t => Digitable t where
  fromDigit :: (Show a , Integral a) => a -> Safe t
