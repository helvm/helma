module HelVM.HelMA.Evaluator where

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
import qualified HelVM.HelMA.Automata.Zot.Evaluator        as Zot

import qualified RIO

runWithOptions :: Has env => RIO.RIO env ()
runWithOptions = runWithOpt =<< optionsRio where
  runWithOpt opt = runLang (App.langCommand opt) opt

runLang :: Has env => LangCommand -> App.AppOptions -> RIO.RIO env ()
-- Implerative
runLang (BFCommand     t) = BF.runWithOptions t
runLang (ETACommand    i) = ETA.runWithOptions i
runLang (PietCommand c l) = Piet.runWithOptions c l
runLang FCommand          = F.runWithOptions
runLang SQCommand         = SQ.runWithOptions
runLang (WSCommand     t) = WS.runWithOptions t
-- Functional
runLang LazyCommand       = Lazy.runWithOptions
runLang ZotCommand        = Zot.runWithOptions
runLang RevCommand        = Rev.runWithOptions
runLang CatCommand        = Cat.runWithOptions
