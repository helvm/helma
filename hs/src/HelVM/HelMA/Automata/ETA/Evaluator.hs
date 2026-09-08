module HelVM.HelMA.Automata.ETA.Evaluator
  ( emitCode
  , emitIL
  , emitTL
  , evalParams
  , run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automaton.API.AutomatonType

import           HelVM.HelMA.Automata.ETA.Automaton
import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.Optimizer
import qualified HelVM.HelMA.Automata.ETA.SimpleParams       as S
import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import qualified HelVM.HelMA.Automaton.API.AppOptions        as App
import qualified HelVM.HelMA.Automaton.API.AutomatonOptions  as Automaton
import           HelVM.HelMA.Automaton.API.AutoOptions
import qualified HelVM.HelMA.Automaton.API.Emit              as Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalOptions
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.API.ParserOptions     ( ParserOptions (optLevel) )

import qualified HelVM.HelMA.Automaton.Automaton             as Automaton

import           HelVM.HelMA.Automaton.Eff.AutomatonEff
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra
import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.Collections.SList               as SList
import           HelVM.HelIO.Control.Safe

import           Control.Applicative.Tools

import qualified Data.Sequence                               as Seq

import           Prelude                                     hiding ( divMod )

import qualified RIO

runRio ∷ Has env ⇒ AutomatonType → RIO.RIO env ()
runRio i = runWIthOptions =<< optionsRio where
  runWIthOptions o = run (App.emit o) i . App.evalParams o =<< readSourceFileRio

run ∷ Has env ⇒ Emit.Emit → AutomatonType → EvalParams → RIO.RIO env ()
run Emit.No   i = runAsRIO . evalParams i
run Emit.IL   _ = putLTextLnRio <=< runAsRIO . uncurry emitIL . toInstructionParams
run Emit.TL   _ = putLTextLnRio . emitTL . source
run Emit.Code _ = putLTextLnRio . emitCode . source

emitIL ∷ MonadSafe m ⇒ OptimizationLevel → Source →  m LText
emitIL ol = printIL <.> optimize ol . tokenize

emitTL ∷ Source → LText
emitTL = show . tokenize

emitCode ∷ Source → LText
emitCode = show . readTokens

simpleEval ∷ AppSafeEff m ⇒ S.SimpleParams → m ()
simpleEval p = evalSource (S.implType p) AllOptimizations (S.source p) (S.stackType p) (S.autoOptions p)

----

evalParams ∷ AppSafeEff m ⇒ AutomatonType → EvalParams → m ()
evalParams e p = evalSource e (optLevel $ parserOptions eo) (source p) (stackAutoOptions eo) (autoOptions eo) where
  eo = evalOptions p

evalSource ∷ (AutomatonEff Symbol m) ⇒ AutomatonType → OptimizationLevel →  Source → StackType → AutoOptions → m ()
evalSource automatonType ol source = evalTL automatonType ol (tokenize source)

evalTL ∷ (AutomatonEff Symbol m) ⇒ AutomatonType → OptimizationLevel → TokenList → StackType → AutoOptions → m ()
evalTL Common ol = common ol
evalTL Custom _  = customEval

common ∷ (AutomatonEff Symbol m) ⇒  OptimizationLevel → TokenList → StackType → AutoOptions → m ()
common ol tl s a = flip Automaton.start (Automaton.withDefaultRam s a) =<< optimize ol tl

customEval ∷ (AutomatonEff Symbol m) ⇒ TokenList → StackType → AutoOptions → m ()
customEval tl ListStackType  = eval tl []
customEval tl SeqStackType   = eval tl Seq.empty
customEval tl SListStackType = eval tl SList.sListEmpty

eval ∷ (SAutomatonEff Symbol s m) ⇒ TokenList → s → AutoOptions → m ()
eval tl s (AutoOptions limit dt) = logDump dt =<< runAutomat limit (newMemory tl s)
