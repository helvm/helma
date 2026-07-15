module HelVM.HelMA.Automata.FALSE.Evaluator where

import           HelVM.HelMA.Automata.FALSE.Parser

import qualified HelVM.HelMA.Automaton.API.AppOptions as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import qualified RIO

import           Text.Pretty.Simple

runRio :: Has env => RIO.RIO env ()
runRio = runWIthOptions =<< optionsRio where
  runWIthOptions o = runAsRIO . run (App.emit o) . App.evalParams o =<< readSourceFileRio

run :: AppEff m => Emit -> EvalParams -> m ()
run No = const $ error "FALSE is not supported now"
run IL = putLTextLnEff . pShowNoColor . parseSafe . source
run _  = fallback
