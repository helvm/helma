{-# OPTIONS_GHC -Wno-partial-fields #-}
module HelVM.HelMA.Automata.Piet.Evaluator (
  run,
) where

import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra


run :: AppEff m => Emit -> EvalParams -> m ()
run No = const $ error "Piet is not supported"
run _  = fallback
