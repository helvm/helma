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

runRio :: Has env => RIO.RIO env ()
runRio = runWithOpt =<< optionsRio where
  runWithOpt = runLang . App.langCommand

runLang :: Has env => LangCommand -> RIO.RIO env ()
-- Implerative
runLang (BFCommand     t) = BF.runRio t
runLang (ETACommand    i) = ETA.runRio i
runLang (PietCommand c l) = Piet.runRio c l
runLang FCommand          = F.runRio
runLang SQCommand         = SQ.runRio
runLang (WSCommand     t) = WS.runRio t
-- Functional
runLang LazyCommand       = Lazy.runRio
runLang ZotCommand        = Zot.runRio
runLang RevCommand        = Rev.runRio
runLang CatCommand        = Cat.runRio
