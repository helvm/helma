module HelVM.HelMA.Automata.Cat.Evaluator (
  batchEval,
  evalParams,
  eval
) where

import HelVM.HelMA.Common.API.EvalParams
import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Util

batchEval :: Source -> Output
batchEval = flip eval emptyInput

evalParams :: Evaluator r => EvalParams ->  r
evalParams = eval . source

----

class Evaluator r where
  eval :: Source ->  r

----

instance Evaluator Interact where
  eval = const

----

instance WrapperIO m => Evaluator (m ()) where
  eval = wPutStr
