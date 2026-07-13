module HelVM.HelMA where

import qualified HelVM.HelMA.Automaton.API.AppOptions      as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.Lang

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

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

actualMain :: Has env => App.AppOptions -> RIO.RIO env ()
actualMain o = runAsRIO . run (App.emit o) (App.langWithOptions o) . App.evalParams o =<< readSourceFile (App.exec o) (App.file o)

run :: AppEff m => Emit -> LangWithOptions -> EvalParams -> m ()
-- Implerative
run emit (LangWithOptions BF   i _ _) = BF.run   emit i
run emit (LangWithOptions ETA  _ i _) = ETA.run  emit i
run emit (LangWithOptions Piet _ _ _) = Piet.run emit
run emit (LangWithOptions F    _ _ _) = F.run    emit
run emit (LangWithOptions SQ   _ _ _) = SQ.run   emit
run emit (LangWithOptions WS   _ _ t) = WS.run   emit t
-- Functional
run emit (LangWithOptions Lazy _ _ _) = Lazy.run emit
run emit (LangWithOptions Zot  _ _ _) = Zot.run  emit
run emit (LangWithOptions Rev  _ _ _) = Rev.run  emit
run emit (LangWithOptions Cat  _ _ _) = Cat.run  emit
