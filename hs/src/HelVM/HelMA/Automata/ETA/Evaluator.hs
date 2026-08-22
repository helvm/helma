module HelVM.HelMA.Automata.ETA.Evaluator
  ( evalParams
  , run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.ETA.API.AutomatonType

import           HelVM.HelMA.Automata.ETA.Automaton
import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.Optimizer
import           HelVM.HelMA.Automata.ETA.Parser
import qualified HelVM.HelMA.Automata.ETA.SimpleParams      as S
import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token


import qualified HelVM.HelMA.Automaton.API.AppOptions       as App
import qualified HelVM.HelMA.Automaton.API.AutomatonOptions as Automaton
import           HelVM.HelMA.Automaton.API.AutoOptions
import qualified HelVM.HelMA.Automaton.API.Emit             as Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import qualified HelVM.HelMA.Automaton.Automaton            as Automaton

import           HelVM.HelMA.Automaton.Eff.AutomatonEff
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.Collections.SList              as SList

import qualified Data.Sequence                              as Seq

import           Prelude                                    hiding ( divMod )

import qualified RIO

import           Text.Pretty.Simple

runRio ∷ Has env ⇒ AutomatonType → RIO.RIO env ()
runRio i = runWIthOptions =<< optionsRio where
  runWIthOptions o = run (App.emit o) i . App.evalParams o =<< readSourceFileRio

run ∷ Has env ⇒ Emit.Emit → AutomatonType → EvalParams → RIO.RIO env ()
run Emit.No   i = runAsRIO . evalParams i
run Emit.IL   _ = putLTextLnRio . pShowNoColor . parseSafe . source
run Emit.TL   _ = putLTextLnRio . show . tokenize . source
run Emit.Code _ = putLTextLnRio . show . readTokens . source

simpleEval ∷ AppSafeEff m ⇒ S.SimpleParams → m ()
simpleEval p = evalSource (S.implType p) (S.source p) (S.stackType p) (S.autoOptions p)

----

evalParams ∷ AppSafeEff m ⇒ AutomatonType → EvalParams → m ()
evalParams e p = evalSource e (source p) (stackAutoOptions p) (autoOptions p)

evalSource ∷ (AutomatonEff Symbol m) ⇒ AutomatonType → Source → StackType → AutoOptions → m ()
evalSource automatonType source = evalTL automatonType (tokenize source)

evalTL ∷ (AutomatonEff Symbol m) ⇒ AutomatonType → TokenList → StackType → AutoOptions → m ()
evalTL Fast     = fastEval
evalTL Original = originalEval

fastEval ∷ (AutomatonEff Symbol m) ⇒ TokenList → StackType → AutoOptions → m ()
fastEval tl s a = flip Automaton.start (Automaton.withDefaultRam s a) =<< optimize tl

originalEval ∷ (AutomatonEff Symbol m) ⇒ TokenList → StackType → AutoOptions → m ()
originalEval tl ListStackType  = eval tl []
originalEval tl SeqStackType   = eval tl Seq.empty
originalEval tl SListStackType = eval tl SList.sListEmpty

eval ∷ (SAutomatonEff Symbol s m) ⇒ TokenList → s → AutoOptions → m ()
eval tl s (AutoOptions _ limit dt) = logDump dt =<< runAutomat limit (newMemory tl s)
