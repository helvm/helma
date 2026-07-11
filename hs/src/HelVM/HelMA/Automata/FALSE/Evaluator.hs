module HelVM.HelMA.Automata.FALSE.Evaluator where

import           HelVM.HelMA.Automata.FALSE.Parser

import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.EvalParams

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           Text.Pretty.Simple

run :: AppEff m => Emit -> EvalParams -> m ()
run No = const $ error "FALSE is not supported now"
run IL = ePutLTextLn . pShowNoColor . parseSafe . source
run _  = fallback
