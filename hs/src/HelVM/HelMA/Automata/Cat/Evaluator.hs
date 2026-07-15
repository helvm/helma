module HelVM.HelMA.Automata.Cat.Evaluator (
  runRio,
  run,
  evalParams,
  eval
) where

import qualified HelVM.HelMA.Automaton.API.AppOptions as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Extra

import qualified RIO

runRio :: Has env => RIO.RIO env ()
runRio = runWithOptions =<< optionsRio where
  runWithOptions o = runAsRIO . run (App.emit o) . App.evalParams o =<< readSourceFileRio

run :: AppEff m => Emit -> EvalParams -> m ()
run No = evalParams
run _  = fallback

evalParams :: AppEff m => EvalParams -> m ()
evalParams = eval . source

eval :: MonadEff m => Source -> m ()
eval = putTextEff
