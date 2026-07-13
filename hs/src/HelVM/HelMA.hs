module HelVM.HelMA where

import qualified HelVM.HelMA.Automaton.API.AppOptions      as App
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.Lang

import qualified HelVM.HelMA.Automata.BrainFuck.Evaluator  as BF
import qualified HelVM.HelMA.Automata.Cat.Evaluator        as Cat
import qualified HelVM.HelMA.Automata.ETA.Evaluator        as ETA
import qualified HelVM.HelMA.Automata.FALSE.Evaluator      as F
import qualified HelVM.HelMA.Automata.LazyK.Evaluator      as Lazy
import qualified HelVM.HelMA.Automata.Piet.Evaluator       as Piet
import qualified HelVM.HelMA.Automata.Rev.Evaluator        as Rev
import qualified HelVM.HelMA.Automata.SubLeq.Evaluator     as SQ
import qualified HelVM.HelMA.Automata.WhiteSpace.Evaluator as WS
import qualified HelVM.HelMA.Automata.Zot.Automaton        as Zot

import qualified RIO

runWithOptions :: Has env => App.AppOptions -> RIO.RIO env ()
runWithOptions = runLang . App.lang <*> id

runLang :: Has env => Lang -> App.AppOptions -> RIO.RIO env ()
-- Implerative
runLang BF   = BF.runWithOptions
runLang ETA  = ETA.runWithOptions
runLang Piet = Piet.runWithOptions
runLang F    = F.runWithOptions
runLang SQ   = SQ.runWithOptions
runLang WS   = WS.runWithOptions
-- Functional
runLang Lazy = Lazy.runWithOptions
runLang Zot  = Zot.runWithOptions
runLang Rev  = Rev.runWithOptions
runLang Cat  = Cat.runWithOptions
