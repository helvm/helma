{-# OPTIONS_GHC -Wno-partial-fields #-}
module HelVM.HelMA.Automata.Piet.Evaluator (
  runRio,
  run,
) where

import           HelVM.HelMA.Automata.Piet.API.LexerType

import qualified HelVM.HelMA.Automaton.API.AppOptions    as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams

import           HelVM.HelMA.Automaton.Extra

import qualified RIO

runRio :: Has env => Maybe Natural -> Maybe LexerType -> RIO.RIO env ()
runRio _ _ = runWithOptions =<< optionsRio where
  runWithOptions o = run (App.emit o) . App.evalParams o =<< readSourceFileRio

run :: Has env => Emit -> EvalParams -> RIO.RIO env ()
run No = const $ error "Piet is not supported"
run _  = fallback
