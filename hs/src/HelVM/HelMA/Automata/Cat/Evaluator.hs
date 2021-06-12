module HelVM.HelMA.Automata.Cat.Evaluator (
  evalParams,
  eval
) where

import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.IO.WrapperIO

import HelVM.Common.SafeMonadT

evalParams :: (Monad m , Evaluator (m ())) => EvalParams -> SafeMonadT_ m
evalParams = hoistMonad . eval . source

----

class Evaluator r where
  eval :: Source ->  r

----

instance WrapperIO m => Evaluator (m ()) where
  eval = wPutStr
