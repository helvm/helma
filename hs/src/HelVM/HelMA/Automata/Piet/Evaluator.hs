{-# OPTIONS_GHC -Wno-partial-fields #-}
module HelVM.HelMA.Automata.Piet.Evaluator (
  runWithOptions,
  run,
) where

import           HelVM.HelMA.Automata.Piet.API.LexerType

import qualified HelVM.HelMA.Automaton.API.AppOptions    as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import qualified RIO

runWithOptions :: Has env => Maybe Natural -> Maybe LexerType -> App.AppOptions -> RIO.RIO env ()
runWithOptions _ _ o = runAsRIO . run (App.emit o) . App.evalParams o =<< readSourceFile (App.exec o) (App.file o)

run :: AppEff m => Emit -> EvalParams -> m ()
run No = const $ error "Piet is not supported"
run _  = fallback
