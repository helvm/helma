module HelVM.HelMA.Automata.Cat.Evaluator (
  evalParams,
  eval
) where

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Util

evalParams :: Evaluator r => EvalParams ->  r
evalParams = eval . source

----

class Evaluator r where
  eval :: Source ->  r

----

instance WrapperIO m => Evaluator (m ()) where
  eval = wPutStr
