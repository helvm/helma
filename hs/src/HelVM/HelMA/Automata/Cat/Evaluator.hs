module HelVM.HelMA.Automata.Cat.Evaluator (
  runWithOptions,
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

runWithOptions :: Has env => App.AppOptions -> RIO.RIO env ()
runWithOptions o = runAsRIO . run (App.emit o) . App.evalParams o =<< readSourceFile (App.exec o) (App.file o)

run :: AppEff m => Emit -> EvalParams -> m ()
run No = evalParams
run _  = fallback

evalParams :: AppEff m => EvalParams -> m ()
evalParams = eval . source

eval :: MonadEff m => Source -> m ()
eval = ePutText
