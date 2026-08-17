module HelVM.HelMA.Automata.SubLeq.Evaluator
  ( evalParams
  , run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.SubLeq.Automaton
import           HelVM.HelMA.Automata.SubLeq.Lexer

import qualified HelVM.HelMA.Automaton.API.AppOptions   as App
import qualified HelVM.HelMA.Automaton.API.Emit         as Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.AutomatonEff
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelMA.Automaton.Trampoline

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType

import qualified HelVM.HelIO.Collections.SList          as SList

import qualified Data.Sequence                          as Seq

import qualified RIO

runRio ∷ Has env ⇒ RIO.RIO env ()
runRio = runWithOptions =<< optionsRio where
  runWithOptions o =  run (App.emit o) . App.evalParams o =<< readSourceFileRio

run ∷ Has env ⇒ Emit.Emit → EvalParams → RIO.RIO env ()
run Emit.No   = runAsRIO . evalParams
run Emit.IL   = putLTextLnRio . show . tokenize . source
run Emit.TL   = putLTextLnRio . show . tokenize . source
run Emit.Code = putLTextLnRio . show . readSymbols . source

simpleEval ∷ AppEff m ⇒ RAMType → Source → m ()
simpleEval rt s = evalSource s rt testMaybeLimit Pretty

----

evalParams ∷ AppEff m ⇒ EvalParams → m ()
evalParams p = evalSource (source p) (ramAutoOptions p) Nothing (dumpAutoOptions p)

evalSource ∷ AppEff m ⇒ Source → RAMType → LimitMaybe → DumpType → m ()
evalSource source = evalIL $ tokenize source

evalIL ∷ AutomatonEff e m ⇒ [e] → RAMType → LimitMaybe → DumpType → m ()
evalIL = flip evalIL'

evalIL' ∷ AutomatonEff e m ⇒ RAMType → [e] → LimitMaybe → DumpType → m ()
evalIL' ListRAMType    = start
evalIL' SeqRAMType     = start . Seq.fromList
evalIL' SListRAMType   = start . SList.sListFromList
evalIL' MapListRAMType = start . Seq.fromList

start ∷ RAutomatonEff e r m ⇒ r → LimitMaybe → DumpType → m ()
start r limit dt = logDump dt =<< runAutomat limit (newMemory r)
