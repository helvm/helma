module HelVM.HelMA.Automaton.Operator.IOOperator where

import           HelVM.HelMA.Automaton.Dynamic.DynamicIntegral
import           HelVM.HelMA.Automaton.IO.BusinessIO

doIO:: (BusinessIO m , DynamicIntegral i a) => IOOperator -> [a] -> m [a]
doIO OutputChar (a : s) = s <$ wPutAsChar (fromDynamicIntegralUnsafe a)
doIO OutputDec  (a : s) = s <$ wPutAsDec  (fromDynamicIntegralUnsafe a)
doIO InputChar       s  = ( : s) . toDynamicIntegral <$> wGetCharAs
doIO InputDec        s  = ( : s) . toDynamicIntegral <$> wGetDecAs
doIO _              []  = error "doIO empty"

data IOOperator =
    OutputChar
  | OutputDec
  | InputChar
  | InputDec
  deriving stock (Eq , Show , Read)
