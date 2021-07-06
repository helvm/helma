module HelVM.HelMA.Automata.Cat.Evaluator (
  evalParams,
  eval
) where

import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.IO.BusinessIO

evalParams :: BIO m => EvalParams -> m ()
evalParams = eval . source

eval :: BusinessIO m => Source -> m ()
eval = wPutStr
