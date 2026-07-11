module HelVM.HelMA.Automata.Cat.Evaluator (
  run,
  evalParams,
  eval
) where

import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Extra


run :: AppEff m => Emit -> EvalParams -> m ()
run No = evalParams
run _  = fallback

evalParams :: AppEff m => EvalParams -> m ()
evalParams = eval . source

eval :: MonadEff m => Source -> m ()
eval = ePutText
