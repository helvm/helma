module HelVM.HelMA.Automata.SubLeq.Evaluator (
  simpleEval,
  evalParams,
) where

import           HelVM.HelMA.Automata.SubLeq.Automaton
import           HelVM.HelMA.Automata.SubLeq.Lexer

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.AutomatonEff
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Trampoline

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType

import qualified HelVM.HelIO.Collections.MapList        as MapList
import qualified HelVM.HelIO.Collections.SList          as SList

import qualified Data.Sequence                          as Seq

simpleEval :: AppEff m => RAMType -> Source -> m ()
simpleEval rt s = evalSource s rt testMaybeLimit Pretty

----

evalParams :: AppEff m => EvalParams -> m ()
evalParams p = evalSource (source p) (ramAutoOptions p) Nothing (dumpAutoOptions p)

evalSource :: AppEff m => Source -> RAMType -> LimitMaybe -> DumpType -> m ()
evalSource source = evalIL $ tokenize source

evalIL :: AutomatonEff e m => [e] -> RAMType -> LimitMaybe -> DumpType -> m ()
evalIL = flip evalIL'

evalIL' :: AutomatonEff e m => RAMType -> [e] -> LimitMaybe -> DumpType -> m ()
evalIL' ListRAMType    = start
evalIL' SeqRAMType     = start . Seq.fromList
evalIL' SListRAMType   = start . SList.sListFromList
evalIL' MapListRAMType = start . MapList.mapListFromList

start :: RAutomatonEff e r m => r -> LimitMaybe -> DumpType -> m ()
start r limit dt = logDump dt =<< run limit (newMemory r)
