module HelVM.HelMA.Automata.Cat.Evaluator (
  evalParams,
  eval
) where

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Eff.MonadEff

evalParams :: AppEff m => EvalParams -> m ()
evalParams = eval . source

eval :: MonadEff m => Source -> m ()
eval = ePutText
