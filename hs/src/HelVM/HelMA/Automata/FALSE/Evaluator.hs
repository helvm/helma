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

runWithOptions :: Has env => App.AppOptions -> RIO.RIO env ()
runWithOptions o = runAsRIO . run (App.emit o) . App.evalParams o =<< readSourceFile (App.exec o) (App.file o)

run :: AppEff m => Emit -> EvalParams -> m ()
run No = const $ error "FALSE is not supported now"
run IL = ePutLTextLn . pShowNoColor . parseSafe . source
run _  = fallback
