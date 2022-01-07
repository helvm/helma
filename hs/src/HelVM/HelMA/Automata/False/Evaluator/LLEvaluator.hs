module HelVM.HelMA.Automata.False.Evaluator.LLEvaluator where

import           HelVM.HelMA.Automata.False.API.Version

import qualified HelVM.HelMA.Automata.False.ThulsaDum.Evaluator.LLEvaluator     as TD
import qualified HelVM.HelMA.Automata.False.ThunderSeethe.Evaluator.LLEvaluator as TS

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

eval :: BIO m => Version -> Source -> m ()
eval ThulsaDum     = TD.eval
eval ThunderSeethe = TS.eval
